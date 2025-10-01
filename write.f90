module mesh_writer                                       
  use iso_fortran_env, only: real64                    
contains                                                
  subroutine write_mesh(input_path, output_path, count, ids, x, y, z)
    character(len=*), intent(in) :: input_path, output_path ; integer, intent(in) :: count, ids(:) ! File names / IDs
    real(real64), intent(in) :: x(:), y(:), z(:) ; integer :: in_unit, out_unit, ios, i            ! Coordinates / working vars
    character(len=256) :: line ; logical :: nodes                                                 ! Line buffer / state flag
    open(newunit=in_unit, file=input_path, status='old', action='read', iostat=ios) ; if (ios /= 0) return  ! Open source
    open(newunit=out_unit, file=output_path, status='replace', iostat=ios) ; if (ios /= 0) then ; close(in_unit) ; return ; end if  ! Prepare output
    nodes = .false. ; do                                                                          ! Main copy loop
      read(in_unit, '(A)', iostat=ios) line ; if (ios /= 0) exit ; line = adjustl(line)           ! Grab next line
      if (.not. nodes) then                                                                       ! Outside node block
        write(out_unit, '(A)') trim(line) ; if (index(line, '*NODE') == 1) then                   ! Copy line / detect header
          nodes = .true. ; do i = 1, count                                                        ! Enter node replacement
            write(out_unit, '(I0,3(", ",ES12.5))') ids(i), x(i), y(i), z(i)                     ! Emit translated node
          end do ; end if
      else                                                                                        ! Inside node block
        if (line(1:1) == '*') then ; nodes = .false. ; write(out_unit, '(A)') trim(line) ; end if ! Resume copying on next keyword
      end if
    end do ; close(in_unit) ; close(out_unit)                                                    
  end subroutine write_mesh
end module mesh_writer                                      
