module LinkedList
   type node
      integer data
      type( node ), pointer :: next
   end type node

   contains

   function list_dispose(list) 
     type( node ), pointer :: list, current, previous

     ! Output the list, deallocating them after use.

     !print *, 'List elements are:'

     current => list
     do while ( associated( current ) )
        !print *, current%data
        previous => current
        current => current%next
        deallocate( previous )
     end do
   end function list_dispose

 end module
