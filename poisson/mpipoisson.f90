program name
    use mpi
    implicit none
    integer,PARAMETER::steps=10000,nx=600,ny=600,mysize=150
    INTEGER:: myid,num,rc,ierr,i,j,n
    real(4)::dx,dy
    INTEGER::left,right,tag1,tag2
    integer :: begin,end
    REAL(8):: START1,FINISH1,time
    real(4) ::xmin,xmax,ymin,ymax
    ! real(4),ALLOCATABLE::p(:,:),pn(:,:),b(:,:),pgather(:,:)
    integer :: status(MPI_STATUS_SIZE) 
    real(4)::p(nx,mysize+2),pn(nx,mysize+2),b(nx,mysize+2),pgather(nx,ny)
    xmin=0
    xmax=2
    ymin=0
    ymax=2
    dx=(xmax-xmin)/(nx-1)
    dy=(ymax-ymin)/(ny-1)

    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,num,ierr)
    CALL CPU_TIME(START1)
    
    p=0
    pn=0
    b=0
    if(myid==0)then
        p(:,2)=0
        b(500,80)=100
    end if
    if(myid==(num-1))then
        p(:,mysize+1)=0
        b(100,20)=-100
    end if

    p(1,:)=0
    p(nx,:)=0

    if(myid>0)then
        left=myid-1
    else
        left=MPI_PROC_NULL
    end if

    if(myid<(num-1))then
        right=myid+1
    else
        right=MPI_PROC_NULL
    endif

    ! pn=p

    tag1=3
    tag2=4
    do n=1,steps
        call MPI_SENDRECV(p(1,2),nx,MPI_REAL,left,tag1, &
        p(1,mysize+2),nx,mpi_real,right,tag1,MPI_COMM_WORLD,status,ierr)
        call MPI_SENDRECV(p(1,mysize+1),nx,MPI_REAL,right,tag2,& 
        p(1,1),nx,MPI_REAL,left,tag2,MPI_COMM_WORLD,status,ierr) 
        
        begin=2 
        end=mysize+1 
        
        if(myid==0) then 
            begin=3 
        end if 

        if(myid==(num-1)) then 
            end=mysize 
        end if 

        do j=begin,end
            do i=2,nx-1 
                pn(i,j)=((p(i+1,j)+p(i-1,j))*dy**2+(p(i,j+1)+p(i,j-1))*dx**2-b(i,j)*dx**2*dy**2)/2/(dx**2+dy**2)
            end do 
        end do 

        p=pn!效率更高
        ! do j=begin,end   ！循环会增加运算时间。
        !     do i=2,nx-1 
        !          p(i,j)=pn(i,j)
        !     end do 
        ! end do 
    end do
    
    if(myid/=0) then
        call MPI_SEND(p(1,2),nx*mysize,MPI_REAL,0,tag1,MPI_COMM_WORLD,ierr)
        ! CALL CPU_TIME(FINISH1)
        ! time=FINISH1-START1
        ! print *,'CPU_TIME运算时间为(s):',time,myid
    endif

    if(myid==0) then
     do j=2,mysize+1
        pgather(:,j-1)=p(:,j)
     enddo

     do i=1,(num-1)
        call MPI_RECV(pgather(1,mysize*i+1),nx*mysize,MPI_REAL,i,tag1,MPI_COMM_WORLD,status,ierr)
     end do

     OPEN(100,file='poisson.dat',status="unknown")
     do i=1,nx
        write(100,25) (pgather(i,:))
     end do
     25  FORMAT(*(f9.5))

     CALL CPU_TIME(FINISH1)
     time=FINISH1-START1
     print *,'CPU_TIME运算时间为(s):',time,myid
    end if

    call MPI_FINALIZE(rc) 

end program name