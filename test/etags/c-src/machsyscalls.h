SYSCALL (mach_msg_trap, -25,
	 mach_msg_return_t,
	 (msg, option, send_size,
	  rcv_size, rcv_name, timeout, notify),
	 (mach_msg_header_t *msg,
	  mach_msg_option_t option,
	  mach_msg_size_t send_size,
	  mach_msg_size_t rcv_size,
	  mach_port_t rcv_name,
	  mach_msg_timeout_t timeout,
	  mach_port_t notify))

SYSCALL (mach_reply_port, -26,
	 mach_port_t,
	 (),
	 (void))

SYSCALL (mach_thread_self, -27,
	 mach_port_t,
	 (),
	 (void))

SYSCALL (mach_task_self, -28,
	 mach_port_t,
	 (),
	 (void))

SYSCALL (mach_host_self, -29,
	 mach_port_t,
	 (),
	 (void))
