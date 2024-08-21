subroutine SwanPrintGridInfo
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 2008  Delft University of Technology
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation; either version 2 of
!     the License, or (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     A copy of the GNU General Public License is available at
!     http://www.gnu.org/copyleft/gpl.html#SEC3
!     or by writing to the Free Software Foundation, Inc.,
!     59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!
!   Authors
!
!   40.80: Marcel Zijlema
!
!   Updates
!
!   40.80, July 2007: New subroutine
!
!   Purpose
!
!   Prints some relevant information concerning the grid
!
!   Modules used
!
    use ocpcomm4
    use SwanGriddata
    use SwanGridobjects
!
    implicit none
!
!   Local variables
!
    integer                               :: i         ! loop counter
    integer, save                         :: ient = 0  ! number of entries in this subroutine
    integer                               :: iface     ! actual face of the present cell
    integer                               :: j         ! loop counter
    integer                               :: ncellsb   ! number of boundary cells
    integer                               :: ncellsi   ! number of internal cells
    integer                               :: nfacesb   ! number of boundary faces
    integer                               :: nfacesi   ! number of internal faces
    !
    real                                  :: area      ! area of cell
    real                                  :: gridsize  ! actual gridsize of cell
    real, dimension(3)                    :: h         ! altitudes of triangle
    !
    character(80), dimension(2)           :: helptxt   ! auxiliary textlines
    !
    type(celltype), dimension(:), pointer :: cell      ! datastructure for cells with their attributes
    type(facetype), dimension(:), pointer :: face      ! datastructure for faces with their attributes
!
!   Structure
!
!   Description of the pseudo code
!
!   Source text
!
    if (ltrace) call strace (ient,'SwanPrintGridInfo')
    !
    ! point to cell and face objects
    !
    cell => gridobject%cell_grid
    face => gridobject%face_grid
    !
    if ( grid_generator == meth_adcirc ) then
       !
       ! grid is generated by SMS/ADCIRC
       !
       write (helptxt(1),'(a)') 'solely triangles'
       write (helptxt(2),'(a)') 'SMS/ADCIRC'
       !
    elseif ( grid_generator == meth_triangle ) then
       !
       ! grid is generated by Triangle
       !
       write (helptxt(1),'(a)') 'solely triangles'
       write (helptxt(2),'(a)') 'Triangle'
       !
    elseif ( grid_generator == meth_easy ) then
       !
       ! grid is generated by Easymesh
       !
       write (helptxt(1),'(a)') 'solely triangles'
       write (helptxt(2),'(a)') 'Easymesh'
       !
    else
       !
       write (helptxt(1),'(a)') 'triangles or hybrid cells'
       write (helptxt(2),'(a)') 'unknown'
       !
    endif
    !
    ! determine number of boundary faces and boundary cells
    !
    nfacesb = 0
    do i = 1, nfaces
       if ( face(i)%atti(FMARKER) == 1 ) nfacesb = nfacesb + 1
    enddo
    nfacesi = nfaces - nfacesb
    ncellsb = 0
    do i = 1, ncells
       if ( cell(i)%atti(CMARKER) == 1 ) ncellsb = ncellsb + 1
    enddo
    ncellsi = ncells - ncellsb
    !
    ! write some constant with respect to the grid to PRINT file
    !
    write(PRINTF,100) trim(helptxt(1)), trim(helptxt(2)), nverts, ncells, &
                      ncellsi, ncellsb, nfaces, nfacesi, nfacesb
    !
    ! determine minimum and maximum gridsize of the grid
    !
    mingsiz = 1.0e10
    maxgsiz = 0.0
    !
    do i = 1, ncells
       !
       ! area of cell
       !
       area = cell(i)%attr(CELLAREA)
       !
       ! altitude of cell
       !
       do j = 1, cell(i)%nof
          iface  = cell(i)%face(j)%atti(FACEID)
          h(j) = 2.*area/face(iface)%attr(FACELEN)
       enddo
       !
       ! compute gridsize of current cell
       !
       gridsize = 1./sqrt((h(1)**(-2)+h(2)**(-2)+h(3)**(-2))/3.)
       !
       ! determine minimum and maximum gridsize
       !
       if ( gridsize < mingsiz ) then
          mingsiz = gridsize
       elseif ( gridsize > maxgsiz ) then
          maxgsiz = gridsize
       endif
       !
    enddo
    !
    write(PRINTF,200) mingsiz
    write(PRINTF,300) maxgsiz
    !
    ! format statements
    !
 100 format(// ' The unstructured grid contains ',a,' generated by ',a//      &
               ' Number of vertices          = ',i6//                         &
               ' Number of cells             = ', i6/                         &
               '    Number of internal cells = ', i6/                         &
               '    Number of boundary cells = ',i6//                         &
               ' Number of faces             = ',i6/                          &
               '    Number of internal faces = ',i6/                          &
               '    Number of boundary faces = ',i6//)
 200 format(' The minimum gridsize =',f12.5)
 300 format(' The maximum gridsize =',f12.5)

end subroutine SwanPrintGridInfo
