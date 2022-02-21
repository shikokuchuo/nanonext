# nanonext - Documentation -----------------------------------------------------

#' Protocols [Documentation]
#'
#' @description Protocols implemented by \{nanonext\}.
#'
#'     For an authoritative guide please refer to the online documentation for
#'     the NNG library at <https://nng.nanomsg.org/man/>.
#'
#' @section Pair (two-way radio):
#'
#'     [protocol, pair] The pair protocol implements a peer-to-peer pattern,
#'     where relationships between peers are one-to-one. Only one peer may be
#'     connected to another peer at a time, but both may speak freely.
#'
#'     Normally, this pattern will block when attempting to send a message
#'     if no peer is able to receive the message.
#'
#' @section Bus (routing):
#'
#'     [protocol, bus] The bus protocol is useful for routing applications or for
#'     building mesh networks where every peer is connected to every other peer.
#'     In this protocol, each message sent by a node is sent to every one of its
#'     directly connected peers. This socket may be used to send and receive
#'     messages. Sending messages will attempt to deliver to each directly
#'     connected peer.
#'
#'     Messages are only sent to directly connected peers. This means that in
#'     the event that a peer is connected indirectly, it will not receive
#'     messages. When using this protocol to build mesh networks, it is
#'     therefore important that a fully-connected mesh network be constructed.
#'
#'     All message delivery in this pattern is best-effort, which means that
#'     peers may not receive messages. Furthermore, delivery may occur to some,
#'     all, or none of the directly connected peers (messages are not delivered
#'     when peer nodes are unable to receive). Hence, send operations will never
#'     block; instead if the message cannot be delivered for any reason it is
#'     discarded.
#'
#' @section Pipeline (one-way pipe):
#'
#'     In the pipeline pattern, pushers distribute messages to pullers, hence
#'     useful for solving producer/consumer problems.
#'
#'     If multiple peers are connected, the pattern attempts to distribute fairly.
#'     Each message sent by a pusher will be sent to one of its peer pullers,
#'     chosen in a round-robin fashion. This property makes this pattern useful
#'     in load-balancing scenarios.
#'
#'     [protocol, push] The push protocol is one half of a pipeline pattern. The
#'     other side is the pull protocol.
#'
#'     [protocol, pull] The pull protocol is one half of a pipeline pattern. The
#'     other half is the push protocol.
#'
#' @section Request/Reply (I ask, you answer):
#'
#'     In a request/reply pattern, a requester sends a message to one replier,
#'     who is expected to reply with a single answer. This is used for
#'     synchronous communications, for example remote procedure calls (RPCs).
#'
#'     The request is resent automatically if no reply arrives, until a reply is
#'     received or the request times out.
#'
#'     [protocol, req] The req protocol is one half of a request/reply pattern.
#'     This socket may be used to send messages (requests), and then to receive
#'     replies. Generally a reply can only be received after sending a request.
#'
#'     [protocol, rep] The rep protocol is one half of a request/reply pattern.
#'     This socket may be used to receive messages (requests), and then to send
#'     replies. Generally a reply can only be sent after receiving a request.
#'
#' @section Publisher/Subscriber (topics & broadcast):
#'
#'     In a publisher/subscriber pattern, a publisher sends data, which is
#'     broadcast to all subscribers. The subscribing applications only see the
#'     data to which they have subscribed.
#'
#'     [protocol, pub] The pub protocol is one half of a publisher/subscriber
#'     pattern. This socket may be used to send messages, but is unable to
#'     receive them.
#'
#'     [protocol, sub] The sub protocol is one half of a publisher/subscriber
#'     pattern. This socket may be used to receive messages, but is unable to
#'     send them.
#'
#' @section Survey (everyone votes):
#'
#'     In a survey pattern, a surveyor sends a survey, which is broadcast to all
#'     peer respondents. The respondents then have a chance to reply (but are
#'     not obliged to reply). The survey itself is a timed event, so that
#'     responses received after the survey has finished are discarded.
#'
#'     [protocol, surveyor] The surveyor protocol is one half of a survey
#'     pattern. This socket may be used to send messages (surveys), and then to
#'     receive replies. A reply can only be received after sending a survey.
#'     A surveyor can normally expect to receive at most one reply from each
#'     responder. (Messages can be duplicated in some topologies, so there is no
#'     guarantee of this.)
#'
#'     [protocol, respondent] The respondent protocol is one half of a survey
#'     pattern. This socket may be used to receive messages, and then to send
#'     replies. A reply can only be sent after receiving a survey, and generally
#'     the reply will be sent to surveyor from whom the last survey was received.
#'
#' @name protocols
NULL

#' Transports [Documentation]
#'
#' @description Transports supported by \{nanonext\}.
#'
#'     For an authoritative guide please refer to the online documentation for
#'     the NNG library at <https://nng.nanomsg.org/man/>.
#'
#'
#' @section Inproc:
#'
#'     The inproc transport provides communication support between sockets
#'     within the same process. This may be used as an alternative to slower
#'     transports when data must be moved within the same process. This
#'     transport tries hard to avoid copying data, and thus is very light-weight.
#'
#'     [\strong{URI, inproc://}] This transport uses URIs using the scheme
#'     inproc://, followed by an arbitrary string of text, terminated by a NUL
#'     byte. inproc://nanonext is a valid example URL.
#'
#'     \itemize{
#'     \item Multiple URIs can be used within the same application, and they
#'     will not interfere with one another.
#'
#'     \item Two applications may also use the same URI without interfering with
#'     each other, and they will be unable to communicate with each other using
#'     that URI.
#'     }
#'
#' @section IPC:
#'
#'     The ipc transport provides communication support between sockets
#'     within different processes on the same host. For POSIX platforms, this is
#'     implemented using UNIX domain sockets. For Windows, this is implemented
#'     using Windows Named Pipes. Other platforms may have different
#'     implementation strategies.
#'
#'     \emph{Traditional Names}
#'
#'     [\strong{URI, ipc://}] This transport uses URIs using the scheme ipc://,
#'     followed by a path name in the file system where the socket or named pipe
#'     should be created.
#'     \itemize{
#'     \item On POSIX platforms, the path is taken literally, and is relative to
#'     the current directory, unless it begins with /, in which case it is
#'     relative to the root directory. For example, ipc://nanonext refers to the
#'     name nanonext in the current directory, whereas ipc:///tmp/nanonext
#'     refers to nanonext located in /tmp.
#'     \item On Windows, all names are prefixed by \\.\ pipe\ and do not reside
#'     in the normal file system - the required prefix is added automatically
#'     by NNG, so you should specify a URL such as ipc://nanonext directly.
#'     }
#'
#'     \emph{UNIX Aliases}
#'
#'     [\strong{URI, unix://}] The unix:// scheme is an alias for ipc:// and can be
#'     used inter-changeably, but only on POSIX systems.The purpose of this scheme
#'     is to support a future transport making use of AF_UNIX on Windows systems,
#'     at which time it will be necessary to discriminate between the Named Pipes
#'     and the AF_UNIX based transports.
#'
#'     \emph{Abstract Names}
#'
#'     [\strong{URI, abstract://}] On Linux, this transport also can support
#'     abstract sockets. Abstract sockets use a URI-encoded name after the
#'     scheme, which allows arbitrary values to be conveyed in the path,
#'     including embedded NUL bytes. abstract://nanonext is a valid example URL.
#'
#'     \itemize{
#'     \item Abstract sockets do not have any representation in the file system,
#'     and are automatically freed by the system when no longer in use. Abstract
#'     sockets ignore socket permissions, but it is still possible to determine
#'     the credentials of the peer.
#'     }
#'
#' @section TCP/IP:
#'
#'     The tcp transport provides communication support between sockets
#'     across a TCP/IP network. Both IPv4 and IPv6 are supported when the
#'     underlying platform also supports it.
#'
#'     [\strong{URI, tcp://}] This transport uses URIs using the scheme tcp://,
#'     followed by an IP address or hostname, followed by a colon and finally
#'     a TCP port number. For example, to contact port 80 on the localhost
#'     either of the following URIs could be used: tcp://127.0.0.1:80 or
#'     tcp://localhost:80.
#'
#'     \itemize{
#'     \item A URI may be restricted to IPv6 using the scheme tcp6://, and may
#'     be restricted to IPv4 using the scheme tcp4://
#'
#'     \item Note: Specifying tcp6:// may not prevent IPv4 hosts from being used
#'     with IPv4-in-IPv6 addresses, particularly when using a wildcard hostname
#'     with listeners. The details of this varies across operating systems.
#'
#'     \item Note: both tcp6:// and tcp4:// are specific to NNG, and might not
#'     be understood by other implementations.
#'
#'     \item It is recommended to use either numeric IP addresses, or names that
#'     are specific to either IPv4 or IPv6 to prevent confusion and surprises.
#'
#'     \item When specifying IPv6 addresses, the address must be enclosed in
#'     square brackets ([]) to avoid confusion with the final colon separating
#'     the port. For example, the same port 80 on the IPv6 loopback address
#'     (::1) would be specified as tcp://[::1]:80.
#'
#'     \item The special value of 0 (INADDR_ANY) can be used for a listener to
#'     indicate that it should listen on all interfaces on the host. A short-hand
#'     for this form is to either omit the address, or specify the asterisk (*)
#'     character. For example, the following three URIs are all equivalent, and
#'     could be used to listen to port 9999 on the host: (1) tcp://0.0.0.0:9999
#'     (2) tcp://*:9999 (3) tcp://:9999
#'     }
#'
#' @section WebSocket:
#'
#'     The ws transport provides communication support between peers across
#'     a TCP/IP network using WebSockets. Both IPv4 and IPv6 are supported when
#'     the underlying platform also supports it.
#'
#'     [\strong{URI, ws://}] This transport uses URIs using the scheme ws://,
#'     followed by an IP address or hostname, optionally followed by a colon and
#'     a TCP port number, optionally followed by a path. (If no port number is
#'     specified then port 80 is assumed. If no path is specified then a path of
#'     / is assumed.) For example, the URI ws://localhost/app/pubsub would use
#'     port 80 on localhost, with the path /app/pubsub.
#'
#'     \itemize{
#'     \item When specifying IPv6 addresses, the address must be enclosed in
#'     square brackets ([]) to avoid confusion with the final colon separating
#'     the port. For example, the same path and port on the IPv6 loopback
#'     address (::1) would be specified as ws://[::1]/app/pubsub.
#'
#'     \item Note: The value specified as the host, if any, will also be used in
#'     the Host: HTTP header during HTTP negotiation.
#'
#'     \item To listen to all ports on the system, the host name may be elided
#'     from the URL on the listener. This will wind up listening to all
#'     interfaces on the system, with possible caveats for IPv4 and IPv6
#'     depending on what the underlying system supports. (On most modern systems
#'     it will map to the special IPv6 address ::, and both IPv4 and IPv6
#'     connections will be permitted, with IPv4 addresses mapped to IPv6
#'     addresses.)
#'
#'     \item This transport makes use of shared HTTP server instances, permitting
#'     multiple sockets or listeners to be configured with the same hostname and
#'     port. When creating a new listener, it is registered with an existing
#'     HTTP server instance if one can be found. Note that the matching
#'     algorithm is somewhat simple, using only a string based hostname or IP
#'     address and port to match. Therefore it is recommended to use only IP
#'     addresses or the empty string as the hostname in listener URLs.
#'
#'     \item All sharing of server instances is only typically possible within
#'     the same process.
#'
#'     \item The server may also be used by other things (for example to serve
#'     static content), in the same process.
#'     }
#'
#' @name transports
NULL

#' Options [Documentation]
#'
#' @description Options that can be set on Sockets, Contexts, Dialers or Listeners.
#'
#'     Some options are only meaningful or supported in certain contexts; for
#'     example there is no single meaningful address for a socket, since sockets
#'     can have multiple dialers and endpoints associated with them.
#'
#'     For an authoritative guide please refer to the online documentation for
#'     the NNG library at <https://nng.nanomsg.org/man/>.
#'
#' @section Global Options:
#'
#'     \itemize{
#'       \item{'reconnect-time-min' [type 'ms']}
#'
#'       {This is the minimum amount of time (milliseconds) to wait before
#'       attempting to establish a connection after a previous attempt has
#'       failed. This can be set on a socket, but it can also be overridden on
#'       an individual dialer. The option is irrelevant for listeners.}
#'
#'       \item{'reconnect-time-max' [type 'ms']}
#'
#'       {This is the maximum amount of time (milliseconds) to wait before
#'       attempting to establish a connection after a previous attempt has
#'       failed. If this is non-zero, then the time between successive
#'       connection attempts will start at the value of 'reconnect-time-min',
#'       and grow exponentially, until it reaches this value. If this value is
#'       zero, then no exponential back-off between connection attempts is done,
#'       and each attempt will wait the time specified by 'reconnect-time-min'.
#'       This can be set on a socket, but it can also be overridden on an
#'       individual dialer. The option is irrelevant for listeners.}
#'
#'     \item{'recv-size-max' [type 'size']}
#'
#'       {This is the maximum message size that the will be accepted from a
#'       remote peer. If a peer attempts to send a message larger than this,
#'       then the message will be discarded. If the value of this is zero, then
#'       no limit on message sizes is enforced. This option exists to prevent
#'       certain kinds of denial-of-service attacks, where a malicious agent
#'       can claim to want to send an extraordinarily large message, without
#'       sending any data. This option can be set for the socket, but may be
#'       overridden for on a per-dialer or per-listener basis.
#'       NOTE: Applications on hostile networks should set this to a non-zero
#'       value to prevent denial-of-service attacks.
#'       NOTE: Some transports may have further message size restrictions.}
#'
#'     \item{'recv-buffer' [type 'int']}
#'
#'       {This is the depth of the socket’s receive buffer as a number of
#'       messages. Messages received by a transport may be buffered until the
#'       application has accepted them for delivery. This value must be an
#'       integer between 0 and 8192, inclusive. NOTE: Not all protocols support
#'       buffering received messages. For example req can only deal with a
#'       single reply at a time.}
#'
#'     \item{'recv-timeout' [type 'ms']}
#'
#'       {This is the socket receive timeout in milliseconds. When no message is
#'       available for receiving at the socket for this period of time, receive
#'       operations will fail with a return value of 5L ('timed out').}
#'
#'     \item{'send-buffer' [type 'int']}
#'
#'       {This is the depth of the socket send buffer as a number of messages.
#'       Messages sent by an application may be buffered by the socket until a
#'       transport is ready to accept them for delivery. This value must be an
#'       integer between 0 and 8192, inclusive.
#'       NOTE: Not all protocols support buffering sent messages; generally
#'       multicast protocols like pub will simply discard messages when they
#'       cannot be delivered immediately.}
#'
#'     \item{'send-timeout' [type 'ms']}
#'
#'       {This is the socket send timeout in milliseconds. When a message cannot
#'       be queued for delivery by the socket for this period of time (such as
#'       if send buffers are full), the operation will fail with a return value
#'       of 5L ('timed out').}
#'
#'     \item{'socket-name' [type 'string']}
#'
#'       {This the socket name. By default this is a string corresponding to the
#'       value of the socket. The string must fit within 64-bytes, including the
#'       terminating NUL byte. The value is intended for application use, and is
#'       not used for anything in the library itself.}
#'
#'     }
#'
#' @section Protocol-specific Options:
#'
#'     \itemize{
#'       \item{'req:resend-time' [type 'ms']}
#'
#'       {(Request protocol) When a new request is started, a timer of this
#'       duration is also started. If no reply is received before this timer
#'       expires, then the request will be resent. (Requests are also
#'       automatically resent if the peer to whom the original request was sent
#'       disconnects, or if a peer becomes available while the requester is
#'       waiting for an available peer.)}
#'
#'       \item{'sub:subscribe' [type 'string']}
#'
#'       {(Subscribe protocol) This option registers a topic that the subscriber
#'       is interested in. Each incoming message is checked against the list of
#'       subscribed topics. If the body begins with the entire set of bytes in
#'       the topic, then the message is accepted. If no topic matches, then the
#'       message is discarded. To receive all messages, set the topic to NULL. }
#'
#'       \item{'sub:unsubscribe' [type 'string']}
#'
#'       {(Subscribe protocol) This option removes a topic from the subscription
#'       list. Note that if the topic was not previously subscribed to with
#'       'sub:subscribe' then an 'entry not found' error will result.}
#'
#'       \item{'sub:prefnew' [type 'bool']}
#'
#'       {(Subscribe protocol) This option specifies the behavior of the
#'       subscriber when the queue is full. When TRUE (the default), the
#'       subscriber will make room in the queue by removing the oldest message.
#'       When FALSE, the subscriber will reject messages if the message queue
#'       does not have room.}
#'
#'     \item{'surveyor:survey-time' [type 'ms']}
#'
#'       {(Surveyor protocol) Duration of surveys. When a new survey is started,
#'       a timer of this duration is also started. Any responses arriving after
#'       this time will be discarded. Attempts to receive after the timer expires
#'       with no other surveys started will result in an 'incorrect state' error.
#'       Attempts to receive when this timer expires will result in a 'timed
#'       out' error.}
#'
#'     }
#'
#' @section Transport-specific Options:
#'
#'     \itemize{
#'       \item{'ipc:permissions' [type 'int']}
#'
#'       {(IPC transport) This option may be applied to a listener to configure
#'       the permissions that are used on the UNIX domain socket created by that
#'       listener. This property is only supported on POSIX systems. The value
#'       is of type int, representing the normal permission bits on a file, such
#'       as 0600 (typically meaning read-write to the owner, and no permissions
#'       for anyone else.) The default is system-specific, most often 0644.}
#'
#'       \item{'tcp-nodelay' [type 'bool']}
#'
#'       {(TCP transport) This option is used to disable (or enable) the use of
#'       Nagle's algorithm for TCP connections. When TRUE (the default),
#'       messages are sent immediately by the underlying TCP stream without
#'       waiting to gather more data. When FALSE, Nagle’s algorithm is enabled,
#'       and the TCP stream may wait briefly in an attempt to coalesce messages.
#'       Nagle’s algorithm is useful on low-bandwidth connections to reduce
#'       overhead, but it comes at a cost to latency. When used on a dialer or
#'       a listener, the value affects how newly created connections will be
#'       configured.}
#'
#'       \item{'tcp-keepalive' [type 'bool']}
#'
#'       {(TCP transport) This option is used to enable the sending of keep-alive
#'       messages on the underlying TCP stream. This option is FALSE by default.
#'       When enabled, if no messages are seen for a period of time, then a zero
#'       length TCP message is sent with the ACK flag set in an attempt to tickle
#'       some traffic from the peer. If none is still seen (after some
#'       platform-specific number of retries and timeouts), then the remote peer
#'       is presumed dead, and the connection is closed. When used on a dialer
#'       or a listener, the value affects how newly created connections will be
#'       configured. This option has two purposes. First, it can be used to
#'       detect dead peers on an otherwise quiescent network. Second, it can be
#'       used to keep connection table entries in NAT and other middleware from
#'       expiring due to lack of activity.}
#'
#'       \item{'ws:request-headers' [type 'string']}
#'
#'       {(WebSocket transport) Concentation of multiple lines terminated by
#'       CRLF sequences, that can be used to add further headers to the HTTP
#'       request sent when connecting. This option can be set on dialers, and
#'       must be done before the transport is started.}
#'
#'       \item{'ws:response-headers' [type 'string']}
#'
#'       {(WebSocket transport) Concatenation of multiple lines terminated by
#'       CRLF sequences, that can be used to add further headers to the HTTP
#'       response sent when connecting. This option can be set on listeners,
#'       and must be done before the transport is started.}
#'
#'     }
#'
#' @name options
NULL

