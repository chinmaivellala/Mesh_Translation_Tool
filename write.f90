!==============================================================
! Subroutine: write_mesh
! Purpose   : Stream the original mesh file to a new output while
!             replacing the NODE block with the translated
!             coordinates stored in the provided mesh object.
! Created by Chinmai Vellala 06/10/2025
! Modified 07/10/2025
!==============================================================
subroutine write_mesh(mesh, params)
  use iso_fortran_env, only : real64
  use mesh_types, only : mesh_data_t
  use parameter_types, only : parameter_store
  implicit none

  type(mesh_data_t), intent(in) :: mesh
  type(parameter_store), intent(in) :: params
  integer :: in_unit, out_unit, ios, i
  character(len=256) :: line
  logical :: in_nodes

  ! Open source mesh for reading
  open(newunit=in_unit, file=params%input_file, status='old', action='read', iostat=ios)
  if (ios /= 0) then
    print *, 'Error opening input for writing: ', trim(params%input_file)
    return
  end if

  ! Create/overwrite destination mesh file
  open(newunit=out_unit, file=params%output_file, status='replace', iostat=ios)
  if (ios /= 0) then
    print *, 'Error opening output file: ', trim(params%output_file)
    close(in_unit)
    return
  end if

  in_nodes = .false.   ! Tracks whether we are currently inside the *NODE section

  ! Copy line-by-line, injecting translated nodes when needed
  do
    read(in_unit, '(A)', iostat=ios) line
    if (ios /= 0) exit
    line = adjustl(line)

    if (.not. in_nodes) then
      write(out_unit, '(A)') trim(line)
      if (index(line, '*NODE') == 1) then
        in_nodes = .true.
        ! Emit translated coordinates for each stored node
        do i = 1, size(mesh%nodes)
          write(out_unit, '(I0,3(", ",ES12.5))') mesh%nodes(i)%id, mesh%nodes(i)%x, mesh%nodes(i)%y, mesh%nodes(i)%z
        end do
      end if
    else
      if (line(1:1) == '*') then
        in_nodes = .false.
        ! Once we hit the next section header, resume copying lines verbatim.
        write(out_unit, '(A)') trim(line)
      end if
    end if
  end do

  close(in_unit)
  close(out_unit)
end subroutine write_mesh
