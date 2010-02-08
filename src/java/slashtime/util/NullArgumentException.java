/*
 * Slashtime, a small program which displays the time in various places.
 *
 * Copyright © 2006-2010 Operational Dynamics Consulting, Pty Ltd
 *
 * The code in this file, and the program it is a part of, is made available
 * to you by its authors as open source software: you can redistribute it
 * and/or modify it under the terms of the GNU General Public License version
 * 2 ("GPL") as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GPL for more details.
 *
 * You should have received a copy of the GPL along with this program. If not,
 * see http://www.gnu.org/licenses/. The authors of this program may be
 * contacted through http://research.operationaldynamics.com/projects/slashtime/.
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
