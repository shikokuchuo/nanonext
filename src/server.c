// Copyright (C) 2022-2024 Hibiki AI Limited <info@hibiki-ai.com>
//
// This file is part of nanonext.
//
// nanonext is free software: you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.
//
// nanonext is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// nanonext. If not, see <https://www.gnu.org/licenses/>.

// nanonext - HTTP REST Sever --------------------------------------------------

#define NANONEXT_HTTP
#define NANONEXT_PROTOCOLS
#include "nanonext.h"

// REST server -----------------------------------------------------------------

// This file contains modified code with the following licence:
//
// Copyright 2018 Staysail Systems, Inc. <info@staysail.tech>
// Copyright 2018 Capitar IT Group BV <info@capitar.com>
//
// This software is supplied under the terms of the MIT License, a
// copy of which should be located in the distribution where this
// file was obtained (LICENSE.txt).  A copy of the license may also be
// found online at https://opensource.org/licenses/MIT.
//

typedef enum {
  SEND_REQ,
  RECV_REP,
} job_state;

typedef struct rest_job {
  nng_aio *        http_aio; // aio from HTTP we must reply to
  nng_http_res *   http_res; // HTTP response object
  job_state        state;    // 0 = sending, 1 = receiving
  nng_msg *        msg;      // request message
  nng_aio *        aio;      // request flow
  nng_ctx          ctx;      // context on the request socket
  struct rest_job *next;     // next on the freelist
} rest_job;

nng_mtx * job_lock;
rest_job *job_freelist;
nng_socket req_sock;

static void fatal(const char *reason, int xc) {
  nano_printf(1, "%s: %s\n", reason, nng_strerror(xc));
}

static void rest_recycle_job(rest_job *job) {

  if (job->http_res != NULL) {
    nng_http_res_free(job->http_res);
    job->http_res = NULL;
  }
  if (job->msg != NULL) {
    nng_msg_free(job->msg);
    job->msg = NULL;
  }
  if (nng_ctx_id(job->ctx) != 0) {
    nng_ctx_close(job->ctx);
  }

  nng_mtx_lock(job_lock);
  job->next = job_freelist;
  job_freelist = job;
  nng_mtx_unlock(job_lock);

}

static void rest_http_fatal(rest_job *job, const char *fmt, int rv) {

  char buf[128];
  nng_aio *aio = job->http_aio;
  nng_http_res *res = job->http_res;

  job->http_res = NULL;
  job->http_aio = NULL;
  snprintf(buf, sizeof(buf), fmt, nng_strerror(rv));
  nng_http_res_set_status(res, NNG_HTTP_STATUS_INTERNAL_SERVER_ERROR);
  nng_http_res_set_reason(res, buf);
  nng_aio_set_output(aio, 0, res);
  nng_aio_finish(aio, 0);
  rest_recycle_job(job);

}

static void rest_job_cb(void *arg) {

  rest_job *job = arg;
  nng_aio *aio = job->aio;
  int xc;

  switch (job->state) {
  case SEND_REQ:
    if ((xc = nng_aio_result(aio))) {
      rest_http_fatal(job, "send REQ failed: %s", xc);
      return;
    }
    job->msg = NULL;
    nng_aio_set_msg(aio, NULL);
    job->state = RECV_REP;
    nng_ctx_recv(job->ctx, aio);
    break;
  case RECV_REP:
    if ((xc = nng_aio_result(aio))) {
      rest_http_fatal(job, "recv reply failed: %s", xc);
      return;
    }
    job->msg = nng_aio_get_msg(aio);
    if ((xc = nng_http_res_copy_data(job->http_res,
                                     nng_msg_body(job->msg),
                                     nng_msg_len(job->msg)))) {
      rest_http_fatal(job, "nng_http_res_copy_data: %s", xc);
      return;
    }
    nng_aio_set_output(job->http_aio, 0, job->http_res);
    nng_aio_finish(job->http_aio, 0);
    job->http_aio = NULL;
    job->http_res = NULL;
    rest_recycle_job(job);
    break;
  default:
    fatal("bad case", NNG_ESTATE);
    break;
  }

}

static rest_job *rest_get_job(void) {

  rest_job *job;

  nng_mtx_lock(job_lock);
  if ((job = job_freelist) != NULL) {
    job_freelist = job->next;
    nng_mtx_unlock(job_lock);
    job->next = NULL;
    return (job);
  }
  nng_mtx_unlock(job_lock);
  if ((job = calloc(1, sizeof(*job))) == NULL) {
    return (NULL);
  }
  if (nng_aio_alloc(&job->aio, rest_job_cb, job) != 0) {
    free(job);
    return (NULL);
  }
  return (job);

}

void rest_handle(nng_aio *aio) {

  struct rest_job *job;
  nng_http_req *req = nng_aio_get_input(aio, 0);
  void *data;
  size_t sz;
  int xc;

  if ((job = rest_get_job()) == NULL) {
    nng_aio_finish(aio, NNG_ENOMEM);
    return;
  }
  if ((xc = nng_http_res_alloc(&job->http_res)) ||
      (xc = nng_ctx_open(&job->ctx, req_sock))) {
    rest_recycle_job(job);
    nng_aio_finish(aio, xc);
    return;
  }

  nng_http_req_get_data(req, &data, &sz);
  job->http_aio = aio;

  if ((xc = nng_msg_alloc(&job->msg, sz))) {
    rest_http_fatal(job, "nng_msg_alloc: %s", xc);
    return;
  }

  memcpy(nng_msg_body(job->msg), data, sz);
  nng_aio_set_msg(job->aio, job->msg);
  job->state = SEND_REQ;
  nng_ctx_send(job->ctx, job->aio);

}

void rest_start(void *arg) {

  const char **addr = (const char **) arg;
  nng_http_server *server;
  nng_http_handler *handler;
  nng_url *url;
  int xc;

  if ((xc = nng_mtx_alloc(&job_lock)))
    fatal("nng_mtx_alloc", xc);

  job_freelist = NULL;

  if ((xc = nng_url_parse(&url, addr[0])))
    fatal("nng_url_parse", xc);

  if ((xc = nng_req0_open(&req_sock)))
    fatal("nng_req0_open", xc);

  if ((xc = nng_dial(req_sock, addr[1], NULL, NNG_FLAG_NONBLOCK)))
    fatal("nng_dial(inproc_url)", xc);

  if ((xc = nng_http_server_hold(&server, url)))
    fatal("nng_http_server_hold", xc);

  if ((xc = nng_http_handler_alloc(&handler, url->u_path, rest_handle)))
    fatal("nng_http_handler_alloc", xc);

  if ((xc = nng_http_handler_set_method(handler, "POST")))
    fatal("nng_http_handler_set_method", xc);

  if ((xc = nng_http_handler_collect_body(handler, true, 1024 * 128)))
    fatal("nng_http_handler_collect_body", xc);

  if ((xc = nng_http_server_add_handler(server, handler)))
    fatal("nng_http_handler_add_handler", xc);

  if ((xc = nng_http_server_start(server)))
    fatal("nng_http_server_start", xc);

  nng_url_free(url);

}

void inproc_server(const char* url) {

  nng_socket s;
  nng_msg *msg;
  int xc;

  if ((xc = nng_rep0_open(&s)) || (xc = nng_listen(s, url, NULL, 0)))
    fatal("unable to set up inproc", xc);

  for (;;) {
    if ((xc = nng_recvmsg(s, &msg, 0)))
      fatal("inproc recvmsg", xc);

    const char *body = nng_msg_body(msg);
    nano_buf buf;
    SEXP res = R_ParseEvalString(body, R_GlobalEnv);
    nano_serialize(&buf, res, R_NilValue);
    nng_msg_clear(msg);
    nng_msg_append(msg, buf.buf, buf.cur);
    if ((xc = nng_sendmsg(s, msg, 0)))
      fatal("inproc sendmsg", xc);

  }

}

SEXP rnng_rest_server(SEXP url) {

  const char *addr[2] = {CHAR(STRING_ELT(url, 0)), "inproc://n-a-n-o-serv"};
  nng_thread *thr;
  int xc;

  if ((xc = nng_thread_create(&thr, rest_start, (void *) addr)))
    ERROR_OUT(xc);

  inproc_server(addr[1]);

  nng_thread_destroy(thr);
  return R_NilValue;

}
