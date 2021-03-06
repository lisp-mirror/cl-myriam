# cl-myriam

`myriam` is a simple attempt at implementing the [actor model](https://en.wikipedia.org/wiki/Actor_model) for secure, distributed programming. Feedback is very welcome!

This is a Common Lisp port of my `myriam` Chicken Scheme egg. It's already at feature parity with its Scheme sister, aside from the ability to send functions/closures (and obviously, no continuations either), and uses ZMQ instead of NNG for messaging, which means we can make use of CurveZMQ for easier encryption and (some) authentication.

The API is very much unstable at the moment.

# Requirements

Myriam stands on the shoulders of giants.

* `libzmq` built with `libsodium`.
* Systems:
  * `bordeaux-threads`
  * `cl-conspack`
  * `lparallel`
  * `pzmq`
  * `serapeum`
  * `str`
  * `usocket`
  * `sha3`

# Usage

`cl-myriam` is on [Quicklisp](https://www.quicklisp.org/beta/), so you can load it with `(ql:quickload :cl-myriam)`.

Otherwise, you can load the `:cl-myriam` system directly and require the `:myriam` package. Everything you need is in there.

Make sure to check the tests folder for examples.

## `spawn`

`(spawn &rest actions) -> (values string thread)`

Spawn an actor and return its address and thread. If no actions are specified, the actor will only react to predefined actions.

## `action`

`(action name task &optional context) -> action`

Create an action for an actor definition. `name` should be a keyword. `task` should be a function with any number of arguments (or no arguments). `context` should be either `:sync`, `:async` or `:any` and defaults to `:async`; it specifies the context in which a task can be executed, e.g. `:sync` tasks will be executed synchronously and it's what you'll want if you expect to get a value out of them.

## `msg`

`(msg head &rest body) -> message`

Create a message to be sent to an actor. `head` should be a keyword corresponding to the name of a task defined in the target actor. Arguments in `body`, if any, will be passed to the task to be executed.

## `send` and `send*`

`(send address msg)`

`(send* address msg) -> (or * (values nil nil))`

Send a message to an actor to be executed either in an asynchronous (`send`) or synchronous (`send*`) context. `msg` should correspond in context, task name and number of arguments with the target task.

## About identities

Identities encapsulate encryption keys. A `self-identity` contains both a public and secret key, while a `public-identity` contains only a public key.

Actors are spawned with the `self-identity` inside `*current-self-identity*` (which means that many actors can share the same identity, if you want). Outgoing messages need `*target-public-identity*` to be set with a `public-identity` for encryption.

### `make-self-identity`

`(make-self-identity) -> self-identity`

Make a `self-identity` with a keypair.

### `self->public-identity`

`(self->public-identity self-id) -> public-identity`

Make a `public-identity` from the public key in `self-id`.

### `with-self-identity`

`(with-self-identity id &body body)`

Execute `body` with `*current-self-identity*` set with `self-identity` `id`.

### `with-target-identity`

`(with-target-identity id &body body)`

Execute `body` with `*target-public-identity*` set with `public-identity` `id`.

## Messaging

### `ping`

`(send address (msg :ping))`

Check if an actor is alive.

### `stop`

`(send address (msg :stop))`

Stop an actor.

### `store`

`(send address (msg :store key datum))`

Store `datum` into an actor's internal storage. `key` should be a keyword.

### `fetch`

`(send address (msg :fetch key))`

Fetch a value from an actor's internal storage, or `nil` of there is no such value. `key` should be a keyword.

### `*self*`

Dynamic variable useful for referring to an actor's self address.

### `*send-timeout*`

Time to wait for a reply before killing the connection.

## Authentication

By default, actors accept all incoming messages. You need to spawn an authenticator to filter out connections based on host and identities.

### `spawn-authenticator`

`(spawn-authenticator accept-p &optional name) -> (values bt:thread name)`

Spawn an authenticator (a ZAP server). `accept-p` should be a predicate which takes an IP address and the public key of the authenticating client (as a byte vector); an incoming connection will be either accepted or rejected based on the result of `accept-p`. Name shoule be a unique string and it can be used for terminating the authenticator. You can only spawn a single authenticator per context, see below.

### `kill authenticator`

`(kill-authenticator name)`

Kill the given authenticator.

### `authenticator-alive-p`

`(authenticator-alive-p name) -> boolean`

Test if the authenticator with the given name is alive.

## Contexts

Contexts allow you to separate actor groups with different authentication parameters. Without contexts, you're limited to one authenticator per application.

``` common-lisp
(defparameter a
  (myr:with-new-context
    (let ((authenticator (myr:spawn-authenticator
                          (lambda (ip key)
                            (declare (ignore key))
                            (string= ip "127.0.0.1")))))
      (declare (ignore authenticator))
      (myr:spawn))))
```

In this example, `a` will only accept connections from `localhost`.

### `with-new-context`

`(with-new-context &body forms)`

Execute `forms` in a fresh context. Mostly used to spawn actors under an authenticator with specific parameters.

## Utilities

### `change-host`

`(change-host address new-host) -> valid-address`

Replace the host in `address` for `new-host`.

## `all-actors`

`(all-actors) -> list`

Get a list of all local actors.

## `stop-all-actors`

`(stop-all-actors)`

Stop all local actors.

# Notes

## Current limitations/caveats

* Sending your own classes inside a message requires you to define the methods `encode-object` and `decode-object`. See https://github.com/conspack/cl-conspack#clos-and-general-objects for more details, it's easier than it sounds.
* Synchronous messages block execution of an actor for the duration of the task.
* No assumptions are made about identity distribution and service (actor) discovery mechanisms.
