.. _topic-low:

Handling low memory
===================

What does it mean to be "low on memory" in a virtual memory operating
system?

How does the MPS behave when it's low on memory? Performance degrades
(due to running out of zones) and then there are emergency
collections.

How can you handle low memory situations gracefully while still
keeping allocation fast and inline?
