        program main
            use INTER7_CANDIDATE_PAIRS_MOD
            implicit none
            character(len=256) :: filename
        
            call get_command_argument(1, filename)
            call test_candidates(filename) 
        
        end program main 
