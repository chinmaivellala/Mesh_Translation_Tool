module mesh_reader                                    ! === Reader Module ===
  use iso_fortran_env, only: real64                   ! --- Dependencies ---
contains                                               ! --- Public Routines ---
  subroutine read_mesh(path, count, ids, x, y, z)     ! === Node Block Loader ===
    character(len=*), intent(in) :: path ; integer, intent(out) :: count  ! Input / output sizes
    integer, allocatable, intent(out) :: ids(:) ; real(real64), allocatable, intent(out) :: x(:), y(:), z(:)  ! Node storage
    integer :: unit, ios, new_id ; real(real64) :: new_x, new_y, new_z    ! Work variables
    character(len=256) :: line ; logical :: nodes                         ! Line buffer / state flag
    allocate(ids(0), x(0), y(0), z(0)) ; count = 0 ; nodes = .false.      ! Initialise empty state
    open(newunit=unit, file=path, status='old', action='read', iostat=ios) ; if (ios /= 0) return  ! Open source deck
    do                                                                    ! Main scan loop
      read(unit, '(A)', iostat=ios) line ; if (ios /= 0) exit ; line = adjustl(line)               ! Get next line
      if (.not. nodes) then                                               ! Hunt for *NODE header
        if (index(line, '*NODE') == 1) then ; nodes = .true. ; cycle ; end if
      end if
      if (len_trim(line) == 0) cycle ; if (line(1:1) == '*') exit         ! Skip blanks / stop on next keyword
      read(line, *) new_id, new_x, new_y, new_z                           ! Parse node entry
      ids = [ids, new_id] ; x = [x, new_x] ; y = [y, new_y] ; z = [z, new_z] ! Append to arrays
    end do
    close(unit) ; count = size(ids)                                       ! Close file / report count
  end subroutine read_mesh
end module mesh_reader                                                    ! === End Reader ===
