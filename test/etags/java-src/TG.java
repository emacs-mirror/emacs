/*
 * @(#)ThreadGroup.java	1.31 97/01/20
 * 
 * Copyright (c) 1995, 1996 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the confidential and proprietary information of Sun
 * Microsystems, Inc. ("Confidential Information").  You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Sun.
 * 
 * SUN MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. SUN SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 * 
 * CopyrightVersion 1.1_beta
 * 
 */

package java.lang;

import java.io.PrintStream;
import sun.misc.VM;

/**
 * A thread group represents a set of threads. In addition, a thread 
 * group can also include other thread groups. The thread groups form 
 * a tree in which every thread group except the initial thread group 
 * has a parent. 
 * <p>
 * A thread is allowed to access information about its own thread 
 * group, but not to access information about its thread group's 
 * parent thread group or any other thread groups. 
 *
 * @author  unascribed
 * @version 1.31, 20 Jan 1997
 * @since   JDK1.0
 */
/* The locking strategy for this code is to try to lock only one level of the
 * tree wherever possible, but otherwise to lock from the bottom up.
 * That is, from child thread groups to parents.
 * This has the advantage of limiting the number of locks that need to be held
 * and in particular avoids having to grab the lock for the root thread group,
 * (or a global lock) which would be a source of contention on a 
 * multi-processor system with many thread groups.
 * This policy often leads to taking a snapshot of the state of a thread group
 * and working off of that snapshot, rather than holding the thread group locked
 * while we work on the children.
 */
public
class ThreadGroup {
    ThreadGroup parent;
    String name;
    int maxPriority;
    boolean destroyed;
    boolean daemon;
    boolean vmAllowSuspension;

    int nthreads;
    Thread threads[];

    int ngroups;
    ThreadGroup groups[];

    /**
     * Creates an empty Thread group that is not in any Thread group. 
     * This method is used to create the system Thread group.
     */
    private ThreadGroup() {	// called from C code
	this.name = "system";
	this.maxPriority = Thread.MAX_PRIORITY;
    }

    /**
     * Constructs a new thread group. The parent of this new group is 
     * the thread group of the currently running thread. 
     *
     * @param   name   the name of the new thread group.
     * @since   JDK1.0
     */
    public ThreadGroup(String name) {
	this(Thread.currentThread().getThreadGroup(), name);
    }

    /**
     * Creates a new thread group. The parent of this new group is the 
     * specified thread group. 
     * <p>
     * The <code>checkAccess</code> method of the parent thread group is 
     * called with no arguments; this may result in a security exception. 
     *
     * @param     parent   the parent thread group.
     * @param     name     the name of the new thread group.
     * @exception  NullPointerException  if the thread group argument is
     *               <code>null</code>.
     * @exception  SecurityException  if the current thread cannot create a
     *               thread in the specified thread group.
     * @see     java.lang.SecurityException
     * @see     java.lang.ThreadGroup#checkAccess()
     * @since   JDK1.0
     */
    public ThreadGroup(ThreadGroup parent, String name) {
	if (parent == null) {
	    throw new NullPointerException();
	}
	parent.checkAccess();
	this.name = name;
	this.maxPriority = parent.maxPriority;
	this.daemon = parent.daemon;
	this.vmAllowSuspension = parent.vmAllowSuspension;
	this.parent = parent;
	parent.add(this);
    }

    /**
     * Returns the name of this thread group.
     *
     * @return  the name of this thread group.
     * @since   JDK1.0
     */
    public final String getName() {
	return name;
    }

    /**
     * Returns the parent of this thread group.
     *
     * @return  the parent of this thread group. The top-level thread group
     *          is the only thread group whose parent is <code>null</code>.
     * @since   JDK1.0
     */
    public final ThreadGroup getParent() {
	return parent;
    }

    /**
     * Returns the maximum priority of this thread group. Threads that are
     * part of this group cannot have a higher priority than the maximum
     * priority.
     *
     * @return  the maximum priority that a thread in this thread group
     *          can have.
     * @since   JDK1.0
     */
    public final int getMaxPriority() {
	return maxPriority;
    }

    /**
     * Tests if this thread group is a daemon thread group. A 
     * daemon thread group is automatically destroyed when its last 
     * thread is stopped or its last thread group is destroyed. 
     *
     * @return  <code>true</code> if this thread group is a daemon thread group;
     *          <code>false</code> otherwise.
     * @since   JDK1.0
     */
    public final boolean isDaemon() {
	return daemon;
    }

    /**
     * Tests if this thread group has not been destroyed.
     *
     * @since   JDK1.1
     */
    public synchronized boolean isDestroyed() {
	return destroyed;
    }

    /**
     * Changes the daemon status of this thread group.
     * <p>
     * First, the <code>checkAccess</code> method of this thread group is 
     * called with no arguments; this may result in a security exception. 
     * <p>
     * A daemon thread group is automatically destroyed when its last 
     * thread is stopped or its last thread group is destroyed. 
     *
     * @param      daemon   if <code>true</code>, marks this thread group as
     *                      a daemon thread group; otherwise, marks this
     *                      thread group as normal.
     * @exception  SecurityException  if the current thread cannot modify
     *               this thread.
     * @see        java.lang.SecurityException
     * @see        java.lang.ThreadGroup#checkAccess()
     * @since      JDK1.0
     */
    public final void setDaemon(boolean daemon) {
	checkAccess();
	this.daemon = daemon;
    }

    /**
     * Sets the maximum priority of the group. 
     * <p>
     * First, the <code>checkAccess</code> method of this thread group is 
     * called with no arguments; this may result in a security exception. 
     * <p>
     * Threads in the thread group that already have a higher priority 
     * are not affected. 
     *
     * @param      pri   the new priority of the thread group.
     * @exception  SecurityException  if the current thread cannot modify
     *               this thread group.
     * @see        java.lang.SecurityException
     * @see        java.lang.ThreadGroup#checkAccess()
     * @since      JDK1.0
     */
    public final void setMaxPriority(int pri) {
	int ngroupsSnapshot;
	ThreadGroup[] groupsSnapshot;
	synchronized (this) {
	    checkAccess();
	    if (pri < Thread.MIN_PRIORITY) {
		maxPriority = Thread.MIN_PRIORITY;
	    } else if (pri < maxPriority) {
		maxPriority = pri;
	    }
	    ngroupsSnapshot = ngroups;
	    if (groups != null) {
		groupsSnapshot = new ThreadGroup[ngroupsSnapshot];
		System.arraycopy(groups, 0, groupsSnapshot, 0, ngroupsSnapshot);
	    } else {
		groupsSnapshot = null;
	    }
	}
	for (int i = 0 ; i < ngroupsSnapshot ; i++) {
	    groupsSnapshot[i].setMaxPriority(pri);
	}
    }

    /**
     * Tests if this thread group is either the thread group 
     * argument or one of its ancestor thread groups. 
     *
     * @param   g   a thread group.
     * @return  <code>true</code> if this thread group is the thread group
     *          argument or one of its ancestor thread groups;
     *          <code>false</code> otherwise.
     * @since   JDK1.0
     */
    public final boolean parentOf(ThreadGroup g) {
	for (; g != null ; g = g.parent) {
	    if (g == this) {
		return true;
	    }
	}
	return false;
    }

    /**
     * Determines if the currently running thread has permission to 
     * modify this thread group. 
     * <p>
     * If there is a security manager, its <code>checkAccess</code> method 
     * is called with this thread group as its argument. This may result 
     * in throwing a <code>SecurityException</code>. 
     *
     * @exception  SecurityException  if the current thread is not allowed to
     *               access this thread group.
     * @see        java.lang.SecurityManager#checkAccess(java.lang.ThreadGroup)
     * @since      JDK1.0
     */
    public final void checkAccess() {
	SecurityManager security = System.getSecurityManager();
	if (security != null) {
	    security.checkAccess(this);
	}
    }

    /**
     * Returns an estimate of the number of active threads in this
     * thread group.
     *
     * @return  the number of active threads in this thread group and in any
     *          other thread group that has this thread group as an ancestor.
     * @since   JDK1.0
     */
    public int activeCount() {
	int result;
	// Snapshot sub-group data so we don't hold this lock
	// while our children are computing.
	int ngroupsSnapshot;
	ThreadGroup[] groupsSnapshot;
	synchronized (this) {
	    if (destroyed) {
		return 0;
	    }
	    result = nthreads;
	    ngroupsSnapshot = ngroups;
	    if (groups != null) {
		groupsSnapshot = new ThreadGroup[ngroupsSnapshot];
		System.arraycopy(groups, 0, groupsSnapshot, 0, ngroupsSnapshot);
	    } else {
		groupsSnapshot = null;
	    }
	}
	for (int i = 0 ; i < ngroupsSnapshot ; i++) {
	    result += groupsSnapshot[i].activeCount();
	}
	return result;
    }

    /**
     * Copies into the specified array every active thread in this 
     * thread group and its subgroups. 
     * <p>
     * An application should use the <code>activeCount</code> method to 
     * get an estimate of how big the array should be. If the array is 
     * too short to hold all the threads, the extra threads are silently 
     * ignored. 
     *
     * @param   list   an array into which to place the list of threads.
     * @return  the number of threads put into the array.
     * @see     java.lang.ThreadGroup#activeCount()
     * @since   JDK1.0
     */
    public int enumerate(Thread list[]) {
	return enumerate(list, 0, true);
    }

    /**
     * Copies into the specified array every active thread in this 
     * thread group. If the <code>recurse</code> flag is 
     * <code>true</code>, references to every active thread in this 
     * thread's subgroups are also included. If the array is too short to 
     * hold all the threads, the extra threads are silently ignored. 
     * <p>
     * An application should use the <code>activeCount</code> method to 
     * get an estimate of how big the array should be. 
     *
     * @param   list      an array into which to place the list of threads.
     * @param   recurse   a flag indicating whether also to include threads
     *                    in thread groups that are subgroups of this
     *                    thread group.
     * @return  the number of threads placed into the array.
     * @see     java.lang.ThreadGroup#activeCount()
     * @since   JDK1.0
     */
    public int enumerate(Thread list[], boolean recurse) {
	return enumerate(list, 0, recurse);
    }

    private int enumerate(Thread list[], int n, boolean recurse) {
	int ngroupsSnapshot = 0;
	ThreadGroup[] groupsSnapshot = null;
	synchronized (this) {
	    if (destroyed) {
		return 0;
	    }
	    int nt = nthreads;
	    if (nt > list.length - n) {
		nt = list.length - n;
	    }
	    if (nt > 0) {
		System.arraycopy(threads, 0, list, n, nt);
		n += nt;
	    }
	    if (recurse) {
		ngroupsSnapshot = ngroups;
		if (groups != null) {
		    groupsSnapshot = new ThreadGroup[ngroupsSnapshot];
		    System.arraycopy(groups, 0, groupsSnapshot, 0, ngroupsSnapshot);
		} else {
		    groupsSnapshot = null;
		}
	    }
	}
	if (recurse) {
	    for (int i = 0 ; i < ngroupsSnapshot ; i++) {
		n = groupsSnapshot[i].enumerate(list, n, true);
	    }
	}
	return n;
    }

    /**
     * Returns an estimate of the number of active groups in this
     * thread group.
     *
     * @return  the number of active thread groups with this thread group as
     *          an ancestor.
     * @since   JDK1.0
     */
    public int activeGroupCount() {
	int ngroupsSnapshot;
	ThreadGroup[] groupsSnapshot;
	synchronized (this) {
	    if (destroyed) {
		return 0;
	    }
	    ngroupsSnapshot = ngroups;
	    if (groups != null) {
		groupsSnapshot = new ThreadGroup[ngroupsSnapshot];
		System.arraycopy(groups, 0, groupsSnapshot, 0, ngroupsSnapshot);
	    } else {
		groupsSnapshot = null;
	    }
	}
	int n = ngroupsSnapshot;
	for (int i = 0 ; i < ngroupsSnapshot ; i++) {
	    n += groupsSnapshot[i].activeGroupCount();
	}
	return n;
    }

    /**
     * Copies into the specified array references to every active 
     * subgroup in this thread group. 
     * <p>
     * An application should use the <code>activeGroupCount</code> 
     * method to get an estimate of how big the array should be. If the 
     * array is too short to hold all the thread groups, the extra thread 
     * groups are silently ignored. 
     *
     * @param   list   an array into which to place the list of thread groups.
     * @return  the number of thread groups put into the array.
     * @see     java.lang.ThreadGroup#activeGroupCount()
     * @since   JDK1.0
     */
    public int enumerate(ThreadGroup list[]) {
	return enumerate(list, 0, true);
    }

    /**
     * Copies into the specified array references to every active 
     * subgroup in this thread group. If the <code>recurse</code> flag is 
     * <code>true</code>, references to all active subgroups of the 
     * subgroups and so forth are also included. 
     * <p>
     * An application should use the <code>activeGroupCount</code> 
     * method to get an estimate of how big the array should be. 
     *
     * @param   list      an array into which to place the list of threads.
     * @param   recurse   a flag indicating whether to recursively enumerate
     *                    all included thread groups.
     * @return  the number of thread groups put into the array.
     * @see     java.lang.ThreadGroup#activeGroupCount()
     * @since   JDK1.0
     */
    public int enumerate(ThreadGroup list[], boolean recurse) {
	return enumerate(list, 0, recurse);
    }

    private int enumerate(ThreadGroup list[], int n, boolean recurse) {
	int ngroupsSnapshot = 0;
	ThreadGroup[] groupsSnapshot = null;
	synchronized (this) {
	    if (destroyed) {
		return 0;
	    }
	    int ng = ngroups;
	    if (ng > list.length - n) {
		ng = list.length - n;
	    }
	    if (ng > 0) {
		System.arraycopy(groups, 0, list, n, ng);
		n += ng;
	    }
	    if (recurse) {
		ngroupsSnapshot = ngroups;
		if (groups != null) {
		    groupsSnapshot = new ThreadGroup[ngroupsSnapshot];
		    System.arraycopy(groups, 0, groupsSnapshot, 0, ngroupsSnapshot);
		} else {
		    groupsSnapshot = null;
		}
	    }
	}
	if (recurse) {
	    for (int i = 0 ; i < ngroupsSnapshot ; i++) {
		n = groupsSnapshot[i].enumerate(list, n, true);
	    }
	}
	return n;
    }

    /**
     * Stops all processes in this thread group. 
     * <p>
     * First, the <code>checkAccess</code> method of this thread group is 
     * called with no arguments; this may result in a security exception. 
     * <p>
     * This method then calls the <code>stop</code> method on all the 
     * threads in this thread group and in all of its subgroups. 
     *
     * @exception  SecurityException  if the current thread is not allowed
     *               to access this thread group or any of the threads in
     *               the thread group.
     * @see        java.lang.SecurityException
     * @see        java.lang.Thread#stop()
     * @see        java.lang.ThreadGroup#checkAccess()
     * @since      JDK1.0
     */
    public final void stop() {
	int ngroupsSnapshot;
	ThreadGroup[] groupsSnapshot;
	synchronized (this) {
	    checkAccess();
	    for (int i = 0 ; i < nthreads ; i++) {
		threads[i].stop();
	    }
	    ngroupsSnapshot = ngroups;
	    if (groups != null) {
		groupsSnapshot = new ThreadGroup[ngroupsSnapshot];
		System.arraycopy(groups, 0, groupsSnapshot, 0, ngroupsSnapshot);
	    } else {
		groupsSnapshot = null;
	    }
	}
	for (int i = 0 ; i < ngroupsSnapshot ; i++) {
	    groupsSnapshot[i].stop();
	}
    }

    /**
     * Suspends all processes in this thread group. 
     * <p>
     * First, the <code>checkAccess</code> method of this thread group is 
     * called with no arguments; this may result in a security exception. 
     * <p>
     * This method then calls the <code>suspend</code> method on all the 
     * threads in this thread group and in all of its subgroups. 
     *
     * @exception  SecurityException  if the current thread is not allowed
     *               to access this thread group or any of the threads in
     *               the thread group.
     * @see        java.lang.SecurityException
     * @see        java.lang.Thread#suspend()
     * @see        java.lang.ThreadGroup#checkAccess()
     * @since      JDK1.0
     */
    public final void suspend() {
	int ngroupsSnapshot;
	ThreadGroup[] groupsSnapshot;
	synchronized (this) {
	    checkAccess();
	    for (int i = 0 ; i < nthreads ; i++) {
		threads[i].suspend();
	    }
	    ngroupsSnapshot = ngroups;
	    if (groups != null) {
		groupsSnapshot = new ThreadGroup[ngroupsSnapshot];
		System.arraycopy(groups, 0, groupsSnapshot, 0, ngroupsSnapshot);
	    } else {
		groupsSnapshot = null;
	    }
	}
	for (int i = 0 ; i < ngroupsSnapshot ; i++) {
	    groupsSnapshot[i].suspend();
	}
    }

    /**
     * Resumes all processes in this thread group. 
     * <p>
     * First, the <code>checkAccess</code> method of this thread group is 
     * called with no arguments; this may result in a security exception. 
     * <p>
     * This method then calls the <code>resume</code> method on all the 
     * threads in this thread group and in all of its sub groups. 
     *
     * @exception  SecurityException  if the current thread is not allowed to
     *               access this thread group or any of the threads in the
     *               thread group.
     * @see        java.lang.SecurityException
     * @see        java.lang.Thread#resume()
     * @see        java.lang.ThreadGroup#checkAccess()
     * @since      JDK1.0
     */
    public final void resume() {
	int ngroupsSnapshot;
	ThreadGroup[] groupsSnapshot;
	synchronized (this) {
	    checkAccess();
	    for (int i = 0 ; i < nthreads ; i++) {
		threads[i].resume();
	    }
	    ngroupsSnapshot = ngroups;
	    if (groups != null) {
		groupsSnapshot = new ThreadGroup[ngroupsSnapshot];
		System.arraycopy(groups, 0, groupsSnapshot, 0, ngroupsSnapshot);
	    } else {
		groupsSnapshot = null;
	    }
	}
	for (int i = 0 ; i < ngroupsSnapshot ; i++) {
	    groupsSnapshot[i].resume();
	}
    }

    /**
     * Destroys this thread group and all of its subgroups. This thread 
     * group must be empty, indicating that all threads that had been in 
     * this thread group have since stopped. 
     *
     * @exception  IllegalThreadStateException  if the thread group is not
     *               empty or if the thread group has already been destroyed.
     * @exception  SecurityException  if the current thread cannot modify this
     *               thread group.
     * @since      JDK1.0
     */
    public final void destroy() {
	int ngroupsSnapshot;
	ThreadGroup[] groupsSnapshot;
	synchronized (this) {
	    checkAccess();
	    if (destroyed || (nthreads > 0)) {
		throw new IllegalThreadStateException();
	    }
	    ngroupsSnapshot = ngroups;
	    if (groups != null) {
		groupsSnapshot = new ThreadGroup[ngroupsSnapshot];
		System.arraycopy(groups, 0, groupsSnapshot, 0, ngroupsSnapshot);
	    } else {
		groupsSnapshot = null;
	    }
	    if (parent != null) {
		destroyed = true;
		ngroups = 0;
		groups = null;
		nthreads = 0;
		threads = null;
	    }
	}
	for (int i = 0 ; i < ngroupsSnapshot ; i += 1) {
	    groupsSnapshot[i].destroy();
	}
	if (parent != null) {
	    parent.remove(this);
	}
    }

    /**
     * Adds the specified Thread group to this group.
     * @param g the specified Thread group to be added
     * @exception IllegalThreadStateException If the Thread group has been destroyed.
     */
    private final void add(ThreadGroup g){
	synchronized (this) {
	    if (destroyed) {
		throw new IllegalThreadStateException();
	    }
	    if (groups == null) {
		groups = new ThreadGroup[4];
	    } else if (ngroups == groups.length) {
		ThreadGroup newgroups[] = new ThreadGroup[ngroups * 2];
		System.arraycopy(groups, 0, newgroups, 0, ngroups);
		groups = newgroups;
	    }
	    groups[ngroups] = g;

	    // This is done last so it doesn't matter in case the
	    // thread is killed
	    ngroups++;
	}
    }

    /**
     * Removes the specified Thread group from this group.
     * @param g the Thread group to be removed
     * @return if this Thread has already been destroyed.
     */
    private void remove(ThreadGroup g) {
	synchronized (this) {
	    if (destroyed) {
		return;
	    }
	    for (int i = 0 ; i < ngroups ; i++) {
		if (groups[i] == g) {
		    ngroups -= 1;
		    System.arraycopy(groups, i + 1, groups, i, ngroups - i);
		    // Zap dangling reference to the dead group so that
		    // the garbage collector will collect it.
		    groups[ngroups] = null;
		    break;
		}
	    }
	    if (nthreads == 0) {
		notifyAll();
	    }
	    if (daemon && (nthreads == 0) && (ngroups == 0)) {
		destroy();
	    }
	}
    }
    
    /**
     * Adds the specified Thread to this group.
     * @param t the Thread to be added
     * @exception IllegalThreadStateException If the Thread group has been destroyed.
     */
    void add(Thread t) {
	synchronized (this) {
	    if (destroyed) {
		throw new IllegalThreadStateException();
	    }
	    if (threads == null) {
		threads = new Thread[4];
	    } else if (nthreads == threads.length) {
		Thread newthreads[] = new Thread[nthreads * 2];
		System.arraycopy(threads, 0, newthreads, 0, nthreads);
		threads = newthreads;
	    }
	    threads[nthreads] = t;

	    // This is done last so it doesn't matter in case the
	    // thread is killed
	    nthreads++;
	}
    }

    /**
     * Removes the specified Thread from this group.
     * @param t the Thread to be removed
     * @return if the Thread has already been destroyed.
     */
    void remove(Thread t) {
	synchronized (this) {
	    if (destroyed) {
		return;
	    }
	    for (int i = 0 ; i < nthreads ; i++) {
		if (threads[i] == t) {
		    System.arraycopy(threads, i + 1, threads, i, --nthreads - i);
		    // Zap dangling reference to the dead thread so that
		    // the garbage collector will collect it.
		    threads[nthreads] = null;
		    break;
		}
	    }
	    if (nthreads == 0) {
		notifyAll();
	    }
	    if (daemon && (nthreads == 0) && (ngroups == 0)) {
		destroy();
	    }
	}
    }

    /**
     * Prints information about this thread group to the standard 
     * output. This method is useful only for debugging. 
     *
     * @since   JDK1.0
     */
    public void list() {
	list(System.out, 0);
    }
    void list(PrintStream out, int indent) {
	int ngroupsSnapshot;
	ThreadGroup[] groupsSnapshot;
	synchronized (this) {
	    for (int j = 0 ; j < indent ; j++) {
		out.print(" ");
	    }
	    out.println(this);
	    indent += 4;
	    for (int i = 0 ; i < nthreads ; i++) {
		for (int j = 0 ; j < indent ; j++) {
		    out.print(" ");
		}
		out.println(threads[i]);
	    }
	    ngroupsSnapshot = ngroups;
	    if (groups != null) {
		groupsSnapshot = new ThreadGroup[ngroupsSnapshot];
		System.arraycopy(groups, 0, groupsSnapshot, 0, ngroupsSnapshot);
	    } else {
		groupsSnapshot = null;
	    }
	}
	for (int i = 0 ; i < ngroupsSnapshot ; i++) {
	    groupsSnapshot[i].list(out, indent);
	}
    }

    /**
     * Called by the Java Virtual Machine when a thread in this 
     * thread group stops because of an uncaught exception. 
     * <p>
     * The <code>uncaughtException</code> method of 
     * <code>ThreadGroup</code> does the following: 
     * <ul>
     * <li>If this thread group has a parent thread group, the
     *     <code>uncaughtException</code> method of that parent is called
     *     with the same two arguments. 
     * <li>Otherwise, this method determines if the <code>Throwable</code>
     *     argument is an instance of <code>ThreadDeath</code>. If so, nothing
     *     special is done. Otherwise, the <code>Throwable</code>'s
     *     <code>printStackTrace</code> method is called to print a stack
     *     backtrace to the standard error stream.
     * </ul>
     * <p>
     * Applications can override this method in subclasses of 
     * <code>ThreadGroup</code> to provide alternative handling of 
     * uncaught exceptions. 
     *
     * @param   t   the thread that is about to exit.
     * @param   e   the uncaught exception.
     * @see     java.lang.System#err
     * @see     java.lang.ThreadDeath
     * @see     java.lang.Throwable#printStackTrace(java.io.PrintStream)
     * @since   JDK1.0
     */
    public void uncaughtException(Thread t, Throwable e) {
	if (parent != null) {
	    parent.uncaughtException(t, e);
	} else if (!(e instanceof ThreadDeath)) {
	    e.printStackTrace(System.err);
	}
    }

    /**
     * Used by VM to control lowmem implicit suspension.
     *
     * @since   JDK1.1
     */
    public boolean allowThreadSuspension(boolean b) {
	this.vmAllowSuspension = b;
	if (!b) {
	    VM.unsuspendSomeThreads();
	}
	return true;
    }

    /**
     * Returns a string representation of this Thread group.
     *
     * @return  a string representation of this thread group.
     * @since   JDK1.0
     */
    public String toString() {
	return getClass().getName() + "[name=" + getName() + ",maxpri=" + maxPriority + "]";
    }
}
