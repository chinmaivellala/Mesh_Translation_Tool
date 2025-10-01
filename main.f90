program mesh_shift                                                 
  use iso_fortran_env, only: real64                                 
  use mesh_reader, only: read_mesh                                  
  use mesh_translate_ops, only: translate_nodes                      
  use mesh_writer, only: write_mesh                                 
  implicit none                                                    
  character(len=*), parameter :: base_dir    = '/cldata/cvellala/Mesh_Translation_Tool/'
  character(len=*), parameter :: input_file  = base_dir // 'Mesh_BLADE_COLD_STRUCTURAL_NegJacobDeleted1.inp'
  character(len=*), parameter :: output_file = base_dir // 'Mesh_BLADE_COLD_STRUCTURAL_NegJacobDeleted2.inp'
  real(real64), parameter :: dx = 10.0_real64, dy = 0.0_real64, dz = 0.0_real64 ! Shifts
  integer, allocatable :: ids(:)                                    
  real(real64), allocatable :: x(:), y(:), z(:)                     
  integer :: node_count                                             
  call read_mesh(input_file, node_count, ids, x, y, z)              
  call translate_nodes(node_count, x, y, z, dx, dy, dz)             
  call write_mesh(input_file, output_file, node_count, ids, x, y, z) 
end program mesh_shift                                              

