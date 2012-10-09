.. _topic-frame:

=================
Allocation frames
=================


Frames have two important features: A single frame identifies a set of objects(those objects that are "allocated in the frame") which can be destroyed (or declared dead) in a pop operation (see <code><a href="#mps_ap_frame_pop">mps_ap_frame_pop</a></code>); They are arranged in a partially ordered sequence (this is important when the pop operation is used). A fuller and more useful description is found in the APstack protocol document (protocol.mps.alloc-point.stack).</p>

<p>Errors can either be because the client hasn't followed the correct protocol in which case there isn't much that we can recommend or else because some needed resource isn't available. The usual course of actions when short of resources is recommended.</p>

What does it mean for the client not to follow the correct protocol?
