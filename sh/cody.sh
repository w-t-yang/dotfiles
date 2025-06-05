alias cody-login="cody auth login"
alias cody-logout="cody auth logout"

cody_commit () {
    p="Write a commit message for the diff changes"
    res=$(git diff --cached | cody chat -m "$p" -)
    msg=$(echo $res | grep -oz '```.*```' | grep -av '```')
    echo "\n\nHere is message to be commited: "
    echo '---'
    echo $msg
    echo '---'
    read option\?"Press 'y' to confirm and commit: "
    if [ "y" = "$option" ]
    then
        git commit -m "$msg"
    fi
}
alias cody-commit="cody_commit"
alias cody-mr="git diff origin/HEAD HEAD | cody chat -m 'Write a merge request title and description for the changes' -"

cody_mr_update() {
    read mr\?"Enter the MR id: "
    if [ -z $mr ]
    then
        return
    fi
    glab mr view $mr | head -3

    read confirm\?"Is this the MR to update? Enter 'y' to continue: "
    if [ -z $confirm ] || [ $confirm != "y" ]
    then
        return
    fi

    txt=$(cody-mr)
    txt=$(echo $txt | sed -n '/^```/,/^```/ p'  | sed '/^```/ d' )
    title=$(echo $txt | head -1)
    description=$(echo $txt | tail -n +2)

    echo ""
    echo "Update the above MR with the following information"
    echo ""
    echo "title: $title"
    echo ""
    echo "description:"
    echo "$description"
    read confirm\?"Enter 'y' to confirm the update: "

    if [ $confirm = "y" ]
    then
        glab mr update $mr -t "$title" -d "$description"
    fi
}
alias cody-mr-update="cody_mr_update"

create_unit_test() {
    file="$1"
    p="Create unit tests for each method implemented in context file using org.junit.jupiter, use reflection for private methods and private variables"
    res=$(cody chat --context-file $file -m "$p")
    res=$(echo $res | grep -oz '```java:[^```]*```')
    fname=$(echo $res | grep -a '```java:' | sed 's/```java://g')
    content=$(echo $res | grep -av '```')
    echo "Writing test file $fname"
    echo $content > $fname
}
alias cody-unit-test="create_unit_test"
