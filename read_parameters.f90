!==============================================================
! File: read_parameters.f90
! Purpose: Fill parameter_store with key/value pairs from params.txt
! Created by Chinmai Vellala 06/10/2025
! Modified 07/10/2025
!==============================================================
subroutine read_params(params)
  use iso_fortran_env, only : real64
  use parameter_types, only : parameter_store
  implicit none
  type(parameter_store), intent(inout) :: params
  integer :: unit, ios
  character(len=256) :: line, key, value

  ! Reset defaults before parsing the control file
  params%dx = 0.0_real64
  params%dy = 0.0_real64
  params%dz = 0.0_real64
  params%input_file = ''
  params%output_file = ''

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
        read(value, *) params%dx
      case ('dy')
        read(value, *) params%dy
      case ('dz')
        read(value, *) params%dz
      case ('input_file')
        params%input_file = value
      case ('output_file')
        params%output_file = value
      end select
    end if
  end do

  close(unit)
end subroutine read_params