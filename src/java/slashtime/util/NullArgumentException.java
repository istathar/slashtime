/*
 * NullArgumentException.java
 * 
 * Copyright (c) 2006-2007 Operational Dynamics Consulting Pty Ltd
 * 
 * The code in this file, and the program it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */
package slashtime.util;

/**
 * Frequently we have methods which take an argument such as String or some
 * other Object and for which we do not accept <code>null</code>.
 * <p>
 * We could well throw NullPointerException but that is better reserved as a
 * signal of programmer error resulting in uninitialized variables. Likewise
 * IllegalArgumentException is better served being used as a signal of a
 * incorrect input.
 */
public class NullArgumentException extends IllegalArgumentException
{
    private static final long serialVersionUID = 1L;

    public NullArgumentException() {
        super();
    }
}
