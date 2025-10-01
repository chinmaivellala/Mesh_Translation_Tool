program mesh_shift                                                 ! === Driver ===
  use iso_fortran_env, only: real64                                 ! --- Precision ---
  use mesh_reader, only: read_mesh                                  ! --- Reader ---
  use mesh_translate_ops, only: translate_nodes                      ! --- Translator ---
  use mesh_writer, only: write_mesh                                 ! --- Writer ---
  implicit none                                                     ! --- No Implicits ---
  character(len=*), parameter :: input_file = 'input.inp'           ! Input path
  character(len=*), parameter :: output_file = 'shifted.inp'        ! Output path
  real(real64), parameter :: dx = 0.0_real64, dy = 0.0_real64, dz = 0.0_real64 ! Shifts
  integer, allocatable :: ids(:)                                    ! Node IDs
  real(real64), allocatable :: x(:), y(:), z(:)                     ! Coordinates
  integer :: node_count                                             ! Node total
  call read_mesh(input_file, node_count, ids, x, y, z)              ! Load mesh
  call translate_nodes(node_count, x, y, z, dx, dy, dz)             ! Apply shift
  call write_mesh(input_file, output_file, node_count, ids, x, y, z) ! Save mesh
end program mesh_shift                                              ! === End Driver ===
