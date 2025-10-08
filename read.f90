!==============================================================
! File: read.f90
! Purpose: Load mesh data from file into mesh_data_t
! Created by Chinmai Vellala 06/10/2025
! Modified 07/10/2025
!==============================================================
subroutine read_mesh(mesh, params)
  use iso_fortran_env, only : real64
  use mesh_types, only : mesh_data_t, node_t, element_t, set_t
  use parameter_types, only : parameter_store
  implicit none

  type(mesh_data_t), intent(inout) :: mesh
  type(parameter_store), intent(in) :: params

  integer :: unit, ios                ! File unit number and I/O status code
  character(len=256) :: line          ! Raw line read from the mesh file
  logical :: in_nodes, in_elements, in_elset, in_nset ! Flags indicating current section
  integer :: current_elset, current_nset ! Index of the active set being filled
  integer :: id, ns(10), pos, comma, val ! Temporary holders while parsing
  integer :: ios_token                ! I/O status for string-to-number conversion
  real(real64) :: x, y, z             ! Node coordinates read from file
  character(len=256) :: chunk         ! Slice of a set line containing one number
  type(set_t) :: new_set              ! Builder record for a new set definition

  ! Reset state before scanning to know in which section we are in 
  in_nodes = .false.
  in_elements = .false.
  in_elset = .false.
  in_nset = .false.
  current_elset = 0
  current_nset = 0

  ! Ensure storage arrays are empty before appending
  if (allocated(mesh%nodes)) deallocate(mesh%nodes)
  if (allocated(mesh%elements)) deallocate(mesh%elements)
  if (allocated(mesh%elsets)) deallocate(mesh%elsets)
  if (allocated(mesh%nsets)) deallocate(mesh%nsets)
  allocate(mesh%nodes(0), mesh%elements(0), mesh%elsets(0), mesh%nsets(0))

  ! Open the mesh file
  open(newunit=unit, file=params%input_file, status='old', action='read', iostat=ios)
  if (ios /= 0) then
    print *, 'Error opening mesh file: ', trim(params%input_file)
    return
  end if

  ! Scan the file line-by-line, tracking the current block
  do
    read(unit, '(A)', iostat=ios) line
    if (ios /= 0) exit
    line = adjustl(line)   ! Shift text to the left so parsing ignores leading spaces

    ! Section headers start with '*'
    if (line(1:1) == '*') then
      in_nodes = .false.
      in_elements = .false.
      in_elset = .false.
      in_nset = .false.

      if (index(line, '*NODE') == 1) then
        in_nodes = .true.
        if (index(line, 'NSET=') > 0) then
          new_set%name = trim(line(index(line, 'NSET=')+5:))
          if (allocated(new_set%ids)) deallocate(new_set%ids)
          allocate(new_set%ids(0))
          mesh%nsets = [mesh%nsets, new_set]
          current_nset = size(mesh%nsets)
        end if
      else if (index(line, '*ELEMENT') == 1) then
        in_elements = .true.
        if (index(line, 'ELSET=') > 0) then
          new_set%name = trim(line(index(line, 'ELSET=')+6:))
          if (allocated(new_set%ids)) deallocate(new_set%ids)
          allocate(new_set%ids(0))
          mesh%elsets = [mesh%elsets, new_set]
          current_elset = size(mesh%elsets)
        end if
      else if (index(line, '*ELSET') == 1) then
        in_elset = .true.
        new_set%name = trim(line(index(line, 'ELSET=')+6:))
        if (allocated(new_set%ids)) deallocate(new_set%ids)
        allocate(new_set%ids(0))
  mesh%elsets = [mesh%elsets, new_set]
  current_elset = size(mesh%elsets)
      else if (index(line, '*NSET') == 1) then
        in_nset = .true.
        new_set%name = trim(line(index(line, 'NSET=')+5:))
        if (allocated(new_set%ids)) deallocate(new_set%ids)
        allocate(new_set%ids(0))
  mesh%nsets = [mesh%nsets, new_set]
  current_nset = size(mesh%nsets)
      end if

      cycle
    end if

  ! Ignore completely blank lines so we do not attempt to read them.
  if (len_trim(line) == 0) cycle

    ! Consume block entries based on the active section type
    if (in_nodes) then
      ! Convert "node_id, x, y, z" from the text line into numbers.
      read(line, *, iostat=ios_token) id, x, y, z
      if (ios_token /= 0) cycle   ! Skip malformed lines instead of crashing
      mesh%nodes = [mesh%nodes, node_t(id, x, y, z)]
      if (current_nset > 0) then
        mesh%nsets(current_nset)%ids = [mesh%nsets(current_nset)%ids, id]
      end if
    else if (in_elements) then
      ! Each element line is "element_id, node1, node2, ..." (10 nodes for C3D10).
      read(line, *, iostat=ios_token) id, ns
      if (ios_token /= 0) cycle
      mesh%elements = [mesh%elements, element_t(id, ns)]
      if (current_elset > 0) then
        mesh%elsets(current_elset)%ids = [mesh%elsets(current_elset)%ids, id]
      end if
    else if (in_elset .or. in_nset) then
      ! Begin scanning this comma-separated list from the first character.
      pos = 1
      do
        ! Stop when we have consumed all characters on the line.
        if (pos > len_trim(line)) exit
        ! Look for the next comma separator starting from the current position.
        comma = index(line(pos:), ',')

        if (comma == 0) then
          chunk = adjustl(line(pos:))      ! Everything from "pos" to the end of the line
          if (len_trim(chunk) == 0) exit
          read(chunk, *, iostat=ios_token) val
          if (ios_token /= 0) exit
          if (in_elset) mesh%elsets(current_elset)%ids = [mesh%elsets(current_elset)%ids, val]
          if (in_nset) mesh%nsets(current_nset)%ids = [mesh%nsets(current_nset)%ids, val]
          exit
        else
          chunk = adjustl(line(pos:pos+comma-2))
          if (len_trim(chunk) /= 0) then
            read(chunk, *, iostat=ios_token) val
            if (ios_token == 0) then
              if (in_elset) mesh%elsets(current_elset)%ids = [mesh%elsets(current_elset)%ids, val]
              if (in_nset) mesh%nsets(current_nset)%ids = [mesh%nsets(current_nset)%ids, val]
            end if
          end if
          ! Move "position" forward to the character just after the comma we processed.
          pos = pos + comma
        end if
      end do
    end if
  end do

  close(unit)
end subroutine read_mesh
