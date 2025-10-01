module mesh_translate_ops                                   
  use iso_fortran_env, only: real64                          
contains                                                     
  subroutine translate_nodes(count, x, y, z, dx, dy, dz)      
    integer, intent(in) :: count                             
    real(real64), intent(inout) :: x(:), y(:), z(:)           
    real(real64), intent(in) :: dx, dy, dz                    
    integer :: i                                              
    do i = 1, count                                           
      x(i) = x(i) + dx                                       
      y(i) = y(i) + dy                                        
      z(i) = z(i) + dz                                       
    end do
  end subroutine translate_nodes
end module mesh_translate_ops                                 
