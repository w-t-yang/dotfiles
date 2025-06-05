# tmux
alias ta="tmux attach"
alias tl="tmux list-sessions"
alias tmux-fix-ssh='eval $(tmux showenv -s SSH_AUTH_SOCK)'
# tmux end

alias du-sh='du -sh ./*'
alias df-hl='df -hl'

alias pod-list='kubectl get pods -w'
pod_set_name () {
    kubectl get pods -o name | awk '{printf "  %d) %s\n", NR, $1;}'
    read option\?"Select pod: "
    names=($(kubectl get pods -o name))
    pod=""
    declare -i i=1
    for n in "$names[@]"
    do
        if [ "$i" = "$option" ]
        then
            pod="$n"
        fi
        i+=1
    done
    export POD_NAME=$pod
}
alias pod-set-name='pod_set_name'
alias pod-describe='kubectl describe $POD_NAME'
alias pod-log='kubectl logs -f -c app $POD_NAME'
alias pod-shell='kubectl exec -i -t $POD_NAME -c app -- /bin/bash'
alias pod-fqdn='kubectl get svc -o yaml | grep fqdn'

alias java-spotless='mvn spotless:apply'
alias java-build='mvn install -DskipTests'
alias java-build-clean='mvn clean install -DskipTests'
alias java-test-all='mvn test'
java_test () {
  mvn test -Dtest="$1"
}
alias java-test="java_test"
alias java-test-report="open target/site/jacoco/index.html"
alias java-dependency-list='mvn dependency:list'
alias java-dependency-updates='mvn versions:display-dependency-updates'

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
