# API

~~~~{.yaml}
/
/thread/:id -- specific thread [^1]
/thread     -- create a thread (POST)
~~~~

# Id routes

* Can retrieve elements by Id as HTML fragments
* Can be used to move rendering to server-side,
  simply AJAX for text/html to render components
* Problem: ids? Ids are _full_ URL, so no probs!
