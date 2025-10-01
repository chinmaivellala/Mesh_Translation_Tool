module mesh_translate_ops                                    ! === Translation Module ===
  use iso_fortran_env, only: real64                           ! --- Dependencies ---
contains                                                      ! --- Public Routines ---
  subroutine translate_nodes(count, x, y, z, dx, dy, dz)      ! === Apply Constant Shift ===
    integer, intent(in) :: count                              ! Node count
    real(real64), intent(inout) :: x(:), y(:), z(:)           ! Coordinates in place
    real(real64), intent(in) :: dx, dy, dz                    ! Shift values
    integer :: i                                              ! Loop index
    do i = 1, count                                           ! Walk every node
      x(i) = x(i) + dx                                        ! Shift X
      y(i) = y(i) + dy                                        ! Shift Y
      z(i) = z(i) + dz                                        ! Shift Z
    end do
  end subroutine translate_nodes
end module mesh_translate_ops                                 ! === End Translation ===
