#!/bin/bash

printf "Welcome guys!!! It will be fun to run my bash script! \nTry to type "explanation" to get instructions and explanations on how to use the script.\n"
#--------------- 1. Check if local repo is up-to-date ------------------------
repo_status(){
git status
echo "Finished checking if local repo is up-to-date! Congratulations!"
}

#--------------- 2. Put all uncommited changes to changes.log ----------------
uncommit_change(){
git diff>changes.log   #Just redirect the output of git diff
#OPTION : to view the created file named changes.log
read -p "Would you like to view the created file named changes.log? (Y/N) " choice
if [ "$choice" == "Y" ];then
     file_content=$(cat changes.log)
     printf "\n------changes.log--------\n$file_content\nEND\n"
fi
}
#--------------- 3. Put each lines with #TODO to todo.log ---------------------
log_todo(){
grep -r --exclude={todo.log,ProjectAnalyze.sh,project.sh,README.md} "#TODO" * >todo.log
#OPTION : to view the created file named todo.log
read -p "Would you like to view the created file named todo.log? (Y/N) " choice
if [ "$choice" == "Y" ];then
     file_content=$(cat todo.log)
     printf "\n------todo.log--------\n$file_content\nEND\n"
fi
}
#--------------- 4. Check Haskell files for syntax errors -------------------
error_search(){
find . -name "*.hs" -type f | xargs ghc -fno-code &>error.log
#OPTION : to view the created file named error.log
read -p "Would you like to view the created file named error.log? (Y/N) " choice
if [ "$choice" == "Y" ];then
     file_content=$(cat error.log)
     printf "\n-------error.log--------\n$file_content\nEND\n"

fi
}

# FEATURE : additional functionalities 

# ------------FEATURE : get current user_name, date and directory ---------
user_info(){

echo "Hello,$LOGNAME"
Date=$(date)
echo "Current date is $Date"
dr=$(pwd)
echo "Current directory is $dr"
}

# ------------FEATURE : get last updated file in the directory ------------
show_newest(){
ls -lrt | grep ^- | awk 'END{print $NF}'
}
# --------FEATURE : get explanation for user to understand the script-------

explanation(){

printf "\n1.repo_status --Informs you if you're local repo is up to date with the remote repo.\n2.uncommit_change --Puts all uncommited changes in a file changes.log.\n3.log_todo --Puts each line from every file of your project with the tag #TODO into a file todo.log.\n4.error_search --Checks all haskell files for syntax errors and puts the results into error.log.\n5.explanation --help menu.\n6.show_newest --Show the last updated file in the directory.\n7.user_info --print the username and the date.\n"
}
