!==============================================================
! File: read_parameters.f90
! Purpose: Store global translation parameters and load them
!          from a simple control file (params.txt).
! Created by Chinmai Vellala
! Date : 06/10/2020
!==============================================================
module parameter_store
  use iso_fortran_env, only : real64
  implicit none
  ! Offsets applied to each node during translation.
  real(real64) :: dx = 0.0_real64, dy = 0.0_real64, dz = 0.0_real64
  ! Paths to the source mesh and the destination mesh file.
  character(len=256) :: input_file = '', output_file = ''
end module parameter_store

subroutine read_params()
  use iso_fortran_env, only : real64
  use parameter_store
  implicit none
  integer :: unit, ios
  character(len=256) :: line, key, value

  ! Reset defaults before parsing the control file
  dx = 0.0_real64
  dy = 0.0_real64
  dz = 0.0_real64
  input_file = ''
  output_file = ''

  ! Open the control file for reading
  open(newunit=unit, file='params.txt', status='old', action='read', iostat=ios)
  if (ios /= 0) then
    print *, 'Error opening params.in'
    return
  end if

  ! Read key/value pairs until EOF
  do
    read(unit, '(A)', iostat=ios) line
    if (ios /= 0) exit
    line = adjustl(line)
    ! Skip empty lines and comment lines (those starting with '#').
    if (len_trim(line) == 0 .or. line(1:1) == '#') cycle
    if (index(line, '=') > 0) then
      ! Everything before '=' is the key, everything after is the value.
      key = trim(adjustl(line(1:index(line, '=')-1)))
      value = trim(adjustl(line(index(line, '=')+1:)))
      ! Route each value to the matching setting.
      select case (key)
      case ('dx')
        read(value, *) dx
      case ('dy')
        read(value, *) dy
      case ('dz')
        read(value, *) dz
      case ('input_file')
        input_file = value
      case ('output_file')
        output_file = value
      end select
    end if
  end do

  close(unit)
end subroutine read_params