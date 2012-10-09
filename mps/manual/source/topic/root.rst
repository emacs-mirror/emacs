.. _topic-root:

Roots
=====



<p><code><a href="#MPS_RM_CONST">MPS_RM_CONST</a></code> is a preprocessor macro defining a constant that can be OR'ed with other <code>MPS_RM_*</code> constants, and passed as the root mode argument to certain root creation functions (<code><a href="#mps_root_create">mps_root_create</a></code>, <code><a href="#mps_root_create_fmt">mps_root_create_fmt</a></code>, <code><a href="#mps_root_create_table">mps_root_create_table</a></code>, <code><a href="#mps_root_create_table_masked">mps_root_create_table_masked</a></code>, <code><a href="#mps_root_create_reg">mps_root_create_reg</a></code> ).</p>


from MPS_RM_PROT:

<p>No page may contain parts of two or more roots with <code><a
href="#MPS_RM_PROT">MPS_RM_PROT</a></code> [how does one prevent
that?]. You mustn't specify <code><a
href="#MPS_RM_PROT">MPS_RM_PROT</a></code> if the client program or
anything other than (this instance of) the MPS is going to protect or
unprotect the relevant pages.</p>

<h4>Internal Notes</h4>

<p>Future meaning: The MPS may place a hardware read and/or write barrier on any pages which any part of the root covers. Format methods and scanning functions (except for the one for this root) may not read or write data in this root. You may specify <code><a href="#MPS_RM_PROT">MPS_RM_PROT</a></code> on a root allocated from the MPS, as long as it's not from a GCd pool. - drj 1997-12-18</p>

<p>This feature is far too technical for most of our clients: we should think about producing some guidelines on how to use it. - pekka 1998-01-27</p>

<p>There may be problems if the client wants the OS to access the root. Lots of OSes can't cope with writing to protected pages. So we'll need to document that caveat too. drj 1998-05-20</p>
