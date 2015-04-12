/** testede.c --- Macro tests with an EDE project
 *
 * Copyright (C) 2015 Eric Ludlam
 *
 * Author: Eric Ludlam <zappo@ballista>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/.
 */

#define macro_used_as_param B

#define macro_with_param(ARG) int ARG (int b);


macro_with_param(A)

macro_with_param(macro_used_as_param)

macro_with_param(EDEPART)

macro_with_param(EDEHEADERVAR)

macro_with_param(EDEHEADERMACRO(E))
