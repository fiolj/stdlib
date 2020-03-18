FC:= gfortran

OPTIM=0

FFLAGS+=-O$(OPTIM)

time_moments$(OPTIM): mod_moment.f90 time_moments.f90
	$(FC) $(FFLAGS) $^ -o $@
