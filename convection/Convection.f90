program name
    implicit none
    real(4), ALLOCATABLE::fx(:,:),  gx(:,:)
    integer :: i,j,nx,ny,nnx,nny
    real(4) :: dx,dt,u,ddx
    nx=151 !空间网格
    ny=301 !时间网格
    dx=0.1
    dt=0.1
    u=-0.5 !速度为1时候，两者重合

    ddx=0.05  !确定理论解的步长
    nnx=301  !确定理论解的网格点数
    nny=ny

    ALLOCATE(fx(nx,ny),gx(nnx,nny))

    !初始值
    fx(:,:)=0
    fx(int(11.0/dx+1):int(13.0/dx+1),1)=1
    gx(:,:)=0
    gx(int(11.0/ddx+1):int(13.0/ddx+1),1)=1

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
    do j=1,nny-1
        do i=1,nnx
            if (i==nnx) then 
                gx(i,j+1)=gx(1,j)
            else
                gx(i,j+1)=gx(i+1,j)
            end if
        end do
    end do


    OPEN(100,File='outfx.dat',status='unknown')
    OPEN(200,File='outgx.dat',status='unknown')
     do j=1,ny
             write(100,25) fx(:,j)
             write(200,35) gx(:,j)
     end do
     CLOSE(100)
     CLOSE(200)
25  format(151f7.3) !注意在white写格式要用引号
35  format(301f7.3)
end program name



