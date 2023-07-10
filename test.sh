#!/bin/bash
# based on: https://stackoverflow.com/questions/10118381/bash-script-to-automatically-test-program-output-c

declare -a file_base=("test1" "test2")
two="_2"


# Loop the array
for file in "${file_base[@]}"; do

    file_in="test/$file.in"             # The in file
    file_out_val="test/$file.out"       # The out file to check against
    file_out_val_2="test/$file$two.out"       # The out file to check against
    file_out_tst="$file.out.tst"   # The outfile from test application
    file_out_tst_2="$file$two.out.tst"   # The outfile from test application

    if [ ! -f "$file_in" ]; then
        printf "In file %s is missing\n" "$file_in"
        continue;
    fi
    if [ ! -f "$file_out_val" ]; then
        printf "Validation file %s is missing\n" "$file_out_val"
        continue;
    fi

    printf "Testing against %s\n" "$file_in"

    ./flp21-fun -1 < "$file_in" > "$file_out_tst"


    IFS=$'\n' read -d '' -r -a lines < "$file_out_val"
    IFS=$'\n' read -d '' -r -a lines2 < "$file_out_tst"
    sorted_a=($(printf '%s\n' "${lines[@]}"|sort))
    sorted_b=($(printf '%s\n' "${lines2[@]}"|sort))

    diff=$(diff <(printf "%s\n" "${sorted_a[@]}") <(printf "%s\n" "${sorted_b[@]}"))

    e_code=$?
    if [ $e_code != 0 ]; then
            printf "TEST -1 FAIL : %d\n" "$e_code"
    else
            printf "TEST -1 OK!\n"
    fi

    ./flp21-fun -2 < "$file_in" > "$file_out_tst_2"
    IFS=$'\n' read -d '' -r -a lines < "$file_out_val_2"
    IFS=$'\n' read -d '' -r -a lines2 < "$file_out_tst_2"

    # check the rest of the file
    sorted_a=($(printf '%s\n' "${lines[@]:1}"|sort))
    sorted_b=($(printf '%s\n' "${lines2[@]:1}"|sort))

    diff=$(diff <(printf "%s\n" "${sorted_a[@]}") <(printf "%s\n" "${sorted_b[@]}"))

    e_code_2=$?
    

    line1=lines[0]
    line2=lines2[0]

    arr_line1=(${line1//,/ })
    arr_line2=(${line1//,/ })

    sorted_a=($(printf '%s\n' "${arr_line1[@]}"|sort))
    sorted_b=($(printf '%s\n' "${arr_line2[@]}"|sort))

    e_code_3=$?

    if [ $e_code_2 != 0 ] && [ $e_code_3 != 0 ]; then
            printf "TEST -2 FAIL : %d\n" "$e_code"
    else
            printf "TEST -2 OK!\n"
    fi

    



done

# Clean exit with status 0
exit 0