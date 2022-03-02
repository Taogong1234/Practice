program main
    use mpi
    implicit none
    INTEGER:: nx,nt,i,j
    real(4)::dx,dt,u,mc1,mc2,x
    real(4),ALLOCATABLE :: fx(:,:),gx(:,:),px(:,:)
    real(4)::a,b,c,d
    real(4)::aa,z,segma,af,bt
    real(4):: GG,FF
    real(4)::ddx
    INTEGER::nnx,nnt
    INTEGER::ierr,myid,num
    nx=201!先定义，再赋值，再使用
    dx=2.0/200.0
    dt=0.5*dx 
    nt=int(2.0/dt)+1
    u=1.0

    ddx=u*dt !理论解步长
    nnx=int(2.0/ddx)+1
    nnt=nt
    ALLOCATE(fx(nx,nt),px(nx,nt),gx(nnx,nnt))

    call mpi_init(ierr)
    call mpi_comm_rank(MPI_COMM_WORLD,myid,ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,num,ierr)
    print*,'进程号为:',myid,'总进程数为:',num

    ! 赋初始值
    aa=0.5
    z=-0.7
    segma=0.005
    af=10.0
    bt=log(2.0)/36.0/segma**2

    do i=1,nx
        x=(i-1)*dx-1
        if (x>=-0.8 .and.x<=-0.6 ) then
            fx(i,1)=1.0/6.0*(GG(x,bt,z-segma) +GG(x,bt,z+segma)+4*GG(x,bt,z))
        elseif (x>=-0.4 .and.x<=-0.2 ) then
            fx(i,1)=1.0
        elseif (x>=0 .and.x<=0.2 ) then
            fx(i,1)=1-ABS(10*(x-0.1))
        elseif (x>=0.4 .and.x<=0.6 ) then
            fx(i,1)=1/6.0*(FF(x,af,aa-segma) +FF(x,af,aa+segma) +4*FF(x,af,aa))
        else
            fx(i,1)=0          
        end if
    end do

    if (myid==0) then
        ! lax格式
        do j = 1,nt-1
            do i = 1,nx
                if ( i/=1.and.i/=nx ) then
                    fx(i,j+1)=0.5*(fx(i+1,j)+fx(i-1,j))-u*dt/2/dx*(fx(i+1,j)-fx(i-1,j))
                elseif(i==1) then
                    fx(i,j+1)=0.5*(fx(i+1,j)+fx(nx-1,j))-u*dt/2/dx*(fx(i+1,j)-fx(nx-1,j))
                else
                    fx(i,j+1)=0.5*(fx(2,j)+fx(i-1,j))-u*dt/2/dx*(fx(2,j)-fx(i-1,j))
                end if
            end do
        end do

        OPEN(100,File='outfx1.dat',status='unknown')
        do j=1,nt
                write(100,25) fx(:,j)
        end do
        CLOSE(100)
    endif

25  format(201f7.3) 

    if (myid==1) then
    ! !lax-wendroff格式
        do j = 1,nt-1
            do i = 1,nx
                if ( i/=1.and.i/=nx ) then
                    fx(i,j+1)=fx(i,j)-u*dt/2/dx*(fx(i+1,j)-fx(i-1,j))+(u*dt)**2/2/dx**2*(fx(i-1,j)+fx(i+1,j)-2*fx(i,j))
                elseif( i==1) then
                    fx(i,j+1)=fx(i,j)-u*dt/2/dx*(fx(i+1,j)-fx(nx-1,j))+(u*dt)**2/2/dx**2*(fx(nx-1,j)+fx(i+1,j)-2*fx(i,j))
                else
                    fx(i,j+1)=fx(i,j)-u*dt/2/dx*(fx(2,j)-fx(i-1,j))+(u*dt)**2/2/dx**2*(fx(i-1,j)+fx(2,j)-2*fx(i,j))
                end if
            end do
        end do
        
        OPEN(200,File='outfx2.dat',status='unknown')
        do j=1,nt
                write(200,25) fx(:,j)
        end do
        CLOSE(200)
    endif

    if(myid==2)then
        ! Maccormack格式
        do j = 1,nt-1
            do i = 1,nx
                if ( i/=1.and.i/=nx ) then
                    mc1=fx(i,j)-u*dt/dx*(fx(i+1,j)-fx(i,j))
                    mc2=fx(i-1,j)-u*dt/dx*(fx(i,j)-fx(i-1,j))
                    fx(i,j+1)=0.5*(fx(i,j)+mc1)-u*dt/2/dx*(mc1-mc2)
                elseif( i==1) then
                    mc1=fx(i,j)-u*dt/dx*(fx(i+1,j)-fx(i,j))
                    mc2=fx(nx-1,j)-u*dt/dx*(fx(i,j)-fx(nx-1,j))
                    fx(i,j+1)=0.5*(fx(i,j)+mc1)-u*dt/2/dx*(mc1-mc2)
                else
                    mc1=fx(i,j)-u*dt/dx*(fx(2,j)-fx(i,j))
                    mc2=fx(i-1,j)-u*dt/dx*(fx(i,j)-fx(i-1,j))
                    fx(i,j+1)=0.5*(fx(i,j)+mc1)-u*dt/2/dx*(mc1-mc2)
                end if
            end do
        end do

        OPEN(300,File='outfx3.dat',status='unknown')
        do j=1,nt
                write(300,25) fx(:,j)
        end do
        CLOSE(300)
    end if

    if(myid==3)then
        ! ! Beam-Warming格式
        do j = 1,nt-1
            do i = 1,nx
                if  ( i==1 ) then
                    fx(i,j+1)=fx(i,j)-u*dt/dx*(fx(i,j)-fx(nx-1,j))-u*dt/2/dx*(1-u*dt/dx)*(fx(i,j)+fx(nx-2,j)-2*fx(nx-1,j))
                elseif( i==2) then
                    fx(i,j+1)=fx(i,j)-u*dt/dx*(fx(i,j)-fx(i-1,j))-u*dt/2/dx*(1-u*dt/dx)*(fx(i,j)+fx(nx-1,j)-2*fx(i-1,j))
                else
                    fx(i,j+1)=fx(i,j)-u*dt/dx*(fx(i,j)-fx(i-1,j))-u*dt/2/dx*(1-u*dt/dx)*(fx(i,j)+fx(i-2,j)-2*fx(i-1,j))
                end if
            end do
        end do
        OPEN(400,File='outfx4.dat',status='unknown')
        do j=1,nt
                write(400,25) fx(:,j)
        end do
        CLOSE(400)
    end if

    if(myid==4)then
        ! 给px赋初始值
        do i=1,nx
            if (i==1) then 
                px(i,1)=(fx(i+1,1)-fx(nx-1,1))/2/dx
            elseif(i==nx) then
                px(i,1)=(fx(2,1)-fx(i-1,1))/2/dx
            else 
                px(i,1)=(fx(i+1,1)-fx(i-1,1))/2/dx
            end if
        end do

        do j = 1,nt-1
            do i = 1,nx
                if  ( i==1 ) then
                a=(px(i,j)+px(nx-1,j))/dx**2-(2*fx(i,j)-2*fx(nx-1,j))/dx**3
                b=(2*px(i,j)+px(nx-1,j))/dx-(3*fx(i,j)-3*fx(nx-1,j))/dx**2
                c=px(i,j)
                d=fx(i,j)
                fx(i,j+1)=a*(-u*dt)**3+b*(-u*dt)**2+c*(-u*dt)+d
                px(i,j+1)=3*a*(-u*dt)**2+2*b*(-u*dt)+c
                else
                    a=(px(i,j)+px(i-1,j))/dx**2-(2*fx(i,j)-2*fx(i-1,j))/dx**3
                    b=(2*px(i,j)+px(i-1,j))/dx-(3*fx(i,j)-3*fx(i-1,j))/dx**2
                    c=px(i,j)
                    d=fx(i,j)
                    fx(i,j+1)=a*(-u*dt)**3+b*(-u*dt)**2+c*(-u*dt)+d
                    px(i,j+1)=3*a*(-u*dt)**2+2*b*(-u*dt)+c
                end if
            end do
        end do
    
    
    
        do i=1,nnx
            x=(i-1)*ddx-1
            if (x>=-0.8 .and.x<=-0.6 ) then
                gx(i,1)=1.0/6.0*(GG(x,bt,z-segma) +GG(x,bt,z+segma)+4*GG(x,bt,z))
            elseif (x>=-0.4 .and.x<=-0.2 ) then
                gx(i,1)=1.0
            elseif (x>=0 .and.x<=0.2 ) then
                gx(i,1)=1-ABS(10*(x-0.1))
            elseif (x>=0.4 .and.x<=0.6 ) then
                gx(i,1)=1/6.0*(FF(x,af,aa-segma) +FF(x,af,aa+segma) +4*FF(x,af,aa))
            else
                gx(i,1)=0          
            end if
        end do
     ! 理论解--------------------------------------------------------------------------------
        do j=1,nnt-1
            do i=1,nnx
                if (i==1) then 
                    gx(i,j+1)=gx(nnx-1,j)
                else
                    gx(i,j+1)=gx(i-1,j)
                end if
            end do
        end do


     ! 输出数据-------------------------------------------------------------------------------
        OPEN(500,File='outfx5.dat',status='unknown')
        OPEN(600,File='outgx.dat',status='unknown')
        do j=1,nt
                write(500,25) fx(:,j)
                write(600,35) gx(:,j)
        end do
        CLOSE(500)
        CLOSE(600)
    endif
    35  format(401f7.3)
    call MPI_Finalize(ierr)

end program main

function GG(xg,btg,zg) 
    implicit none
    real(4) :: xg,btg,zg
    real(4) :: GG
    GG=exp(-btg*(xg-zg)**2)
    RETURN
end function

function FF(xf,aff,aaf) 
    implicit none
    real(4) :: xf,aff,aaf
    real(4) :: FF
    FF=SQRT(MAX((1-aff**2*(xf-aaf)**2),0.0))
    RETURN
end function