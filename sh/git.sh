wy_branch_options() {
    git branch | grep -v '*' \
        | awk '{printf "  %d) %s\n", NR, $1;}'
}

wy_branch_index_to_name() {
    if [ -z $1 ]
    then
        return
    fi

    branches=($(git branch | grep -v '*'))
    declare -i i=1
    for b in "$branches[@]"
    do
        if [ "$i" = "$1" ]
        then
            echo "$b"
        fi
        i+=1
    done
}

wy_branch_push(){
    name=($(whoami))
    branch=($(git branch | grep "*" | awk '{print $2}'))
    echo "Pushing local \"$branch\" -> remote \"$name/$branch\""
    git push -u origin "$branch":"$name/$branch"

    # msg=($(git push -u origin "$branch":"$name/$branch"))
    # echo $msg
    # url=$(echo $msg | grep "new?merge_request" | sed "s/remote:   //g")

    # echo $url
    # if [ -z $url ]
    # then
    #     return
    # fi

    # read option\?"Press 'y' to open the link to create MR: "
    # if [ "$option" = "y" ]
    # then
    #     open "$url"
    # fi
}

wy_branch_delete() {
    name=$1
    if [ -z $name ]
    then
        return
    fi

    if [ "$name" = "trunk" ] || [ "$name" = "master" ] || [ "$name" = "main" ]
    then
        echo "Cannot delete branch $name."
    else
        git branch -D $name
    fi
}

wy_branch() {
    git branch
    read action\?"checkout(c), new branch(n), delete(d, da), push(P): "

    if [ -z $action ]
    then
        return
    elif [ $action = "n" ]
    then
        read name\?"Enter new branch name: "
        git checkout -b $name
    elif [ $action = "c" ]
    then
        wy_branch_options
        read option\?"Select branch to checkout: "
        name=$(wy_branch_index_to_name $option)

        if [ -z $name ]
        then
            return
        fi

        git checkout $name
    elif [ $action = "d" ]
    then
        wy_branch_options
        read option\?"Select branch to delete: "
        name=$(wy_branch_index_to_name $option)
        wy_branch_delete $name
    elif [ $action = "da" ]
    then
        branches=($(git branch | grep -v '*'))
        declare -i i=1
        for b in "$branches[@]"
        do
            wy_branch_delete $b
        done
    elif [ $action = "P" ]
    then
        wy_branch_push
    fi
}

wy_g () {
    git status

    read action\?"add(s, u), branch(b), commit(c), diff(d, dc), log(l, la), pull(p, pr), push(P): "

    if [ -z $action ]
    then
        return
    elif [ $action = "s" ]
    then
        read file\?"Path or file to stage: "
        git add "$file"
    elif [ $action = "u" ]
    then
        read file\?"File to unstage: "
        git reset "$file"
    elif [ $action = "b" ]
    then
        wy_branch
    elif [ $action = "c" ]
    then
        read msg\?"Commit message: "
        git commit -m "$msg"
    elif [ $action = "d" ]
    then
        git diff
    elif [ $action = "dc" ]
    then
        git diff --cached
    elif [ $action = "l" ]
    then
        git log -n 3
    elif [ $action = "la" ]
    then
        git log
    elif [ $action = "p" ]
    then
        git pull
    elif [ $action = "pr" ]
    then
        git pull --rebase
    elif [ $action = "P" ]
    then
        git push
    fi
}

alias g="wy_g"

# git
alias g-add="git add"
alias g-commit="git commit -m"
alias g-diff-cached="git diff --cached"
alias g-diff="git diff"
alias g-log-all="git log"
alias g-log="git log -n 3"
alias g-pull="git pull"
alias g-rebase="git pull --rebase"
alias g-status="git status"
alias g-branch="wy_branch"

wy_new_mr() {
    name=($(whoami))
    branch=($(git branch | grep "*" | awk '{print $2}'))

    url=$(git config --get remote.origin.url)
    url=$(echo $url | sed 's/:/\//g' | sed 's/git@/https:\/\//g' | sed 's/\.git/\/-/g')
    url="$url/merge_requests/new?merge_request%5Bsource_branch%5D=$name%2F$branch"

    # echo "$url"
    open "$url"
}
alias g-new-mr="wy_new_mr"
