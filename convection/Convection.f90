program name
    implicit none
    real(4), ALLOCATABLE::fx(:,:),  gx(:,:)
    integer :: i,j,nx,ny
    real(4) :: dx,dt,u
    nx=151 !空间
    ny=301 !时间
    dx=0.1
    dt=0.1
    u=-0.5 !速度为1时候，两者重合
    ALLOCATE(fx(nx,ny),gx(nx,ny))
    fx(:,:)=0
    fx(int(11.0/dx+1):int(13.0/dx+1),1)=1
    gx=fx

    ! 解析
    do j=1,ny-1
        do i=1,nx
            if ( i==nx ) then
                fx(i,j+1)=-u*dt/dx*(fx(2,j)-fx(i,j))+fx(i,j)
            else
                fx(i,j+1)=-u*dt/dx*(fx(i+1,j)-fx(i,j))+fx(i,j)
            end if
        end do
    end do

    ! 理论
    do j=1,ny-1
        do i=1,nx
            if ( i>=int((11.0+0.1*u*j)/dx+1) .and. i<=int((13.0+0.1*u*j)/dx+1) ) then 
                gx(i,j+1)=1
            else
                gx(i,j+1)=0
            end if
        end do
    end do


    OPEN(100,File='outfx.dat',status='unknown')
    OPEN(200,File='outgx.dat',status='unknown')
     do j=1,ny
             write(100,25) fx(:,j)
             write(200,25) gx(:,j)
     end do
     CLOSE(100)
     CLOSE(100)
25  format(151f7.3) !注意在white写格式要用引号

end program name



