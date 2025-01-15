# tmux
alias ta="tmux attach"
alias tl="tmux list-sessions"
alias tmux-fix-ssh='eval $(tmux showenv -s SSH_AUTH_SOCK)'
# tmux end

alias pod-list='kubectl get pods -w'
alias pod-set-name='POD_NAME=$(kubectl get pods -o name)'
alias pod-describe='kubectl describe $POD_NAME'
alias pod-log='kubectl logs -f -c app $POD_NAME'
alias pod-shell='kubectl exec -i -t $POD_NAME -c app -- /bin/bash'
alias pod-fqdn='kubectl get svc -o yaml | grep fqdn'

alias java-build='mvn install -DskipTests'
alias java-build-clean='mvn clean install -DskipTests'
alias java-test-all='mvn test'
java_test () {
  mvn test -Dtest="$1"
}
alias java-test="java_test"

alias count-files="find . -type f | wc -l"
alias dk-prune="docker system prune -a -f"

decode_base64_url () {
    local len=$((${#1} % 4))
    local result="$1"
    if [ $len -eq 2 ]; then result="$1"'=='
    elif [ $len -eq 3 ]; then result="$1"'='
    fi
    echo "$result" | tr '_-' '/+' | openssl enc -d -base64
}

decode_jwt () {
    decode_base64_url $(echo -n $2 | cut -d "." -f $1) | jq .
}

decode_jwt_with_bearer () {
    # token=$(echo -n $1 | cut -d " " -f 2)
    # decode_jwt token
    token=$(echo -n $1 | cut -d " " -f 2)
    decode_jwt 2 $token
}
