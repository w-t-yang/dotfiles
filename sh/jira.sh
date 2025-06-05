# Integration with Jira, Jira api token needs to be manually requested and stored in ~/.jira
JIRA_FOLDER_NAME=".jira"
JIRA_TOKEN=$(cat ~/$JIRA_FOLDER_NAME/token)
JIRA_ACCOUNT=$(cat ~/$JIRA_FOLDER_NAME/account)
JIRA_ISSUE_URL=$(cat ~/$JIRA_FOLDER_NAME/issue_url)

jira_issue() {
    JIRA_ISSUE="$1"
    mkdir ~/$JIRA_FOLDER_NAME/$JIRA_ISSUE
    local res=$(curl -s --request GET --url $JIRA_ISSUE_URL/$JIRA_ISSUE --user $JIRA_ACCOUNT:$JIRA_TOKEN --header "Accept: application/json" | jq -r ".fields.description")
    echo $res > ~/$JIRA_FOLDER_NAME/$JIRA_ISSUE/description.txt
}

alias jira-issue="jira_issue"
