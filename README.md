# Scaleway servant demo

A quick overview of the API:

~~~~{.yaml}
GET  /           -- get list of threads
GET  /thread/:id -- retrieve a thread by ID
POST /thread/:id -- create a new comment
POST /thread     -- create a new thread
GET  /static     -- retrieve static assets
~~~~

See the `curl` directory for example requests.
