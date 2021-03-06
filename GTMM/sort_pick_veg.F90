!------------------------------------------------------------------------------
!          Harvard University Atmospheric Chemistry Modeling Group            !
!------------------------------------------------------------------------------
!BOP
!
! !IROUTINE: sort_pick_veg
!
! !DESCRIPTION: This subroutine ...
!\\
!\\
! !INTERFACE:
!
SUBROUTINE sort_pick_veg(arr, ind)
!
! !USES:
!
  USE defineConstants
      
  implicit none
!
! !INPUT/OUTPUT PARAMETERS:
!
  REAL*8, dimension(n_veg, n_age_classes), intent(INOUT) :: ind
  REAL*8, dimension(n_veg, n_age_classes), intent(INOUT) :: arr
!
! !REVISION HISTORY:
!  09 Jul 2010 - C. Carouge - Initial version
!EOP
!------------------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
  INTEGER  :: i, j, n, k
  REAL :: a, b
  
  n=size(arr)
  
  DO k=1,n_veg
     DO j=2,n/n_veg
        a=arr(k,j)
        b=ind(k,j)
        DO i=j-1, 1, -1
           IF (arr(k,i) >=a) exit
           arr(k,i+1)=arr(k,i)
           ind(k,i+1)=ind(k,i)
        END DO
        ind(k,i+1)=b
        arr(k,i+1)=a
     END DO
  END DO
  
END SUBROUTINE sort_pick_veg
!EOC
