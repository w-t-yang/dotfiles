alias bk-login-ssh='ssh-add -D && ssh -A $(cat ~/.bk/ssh_domain)'
alias bk-login-bk='bk auth:login'
alias bk-login-dk='docker login $(cat ~/.bk/docker_domain)'
alias bk-login-all='bk-login-ssh && bk-login-bk && bk-login-dk'

alias bk-ctx='bk ctx'
alias bk-ctx-use='bk context:use'
alias bk-ctx-project="bk context | grep -E 'project\s+[^ ]+' | awk '{print \$2}'"
alias bk-ctx-component="bk context | grep -E 'component\s+[^ ]+' | awk '{print \$2}'"

alias bk-clusters="bk cloud:clusters"
alias bk-clusters-use="bk cloud:clusters:use"
alias bk-clusters-use-dev="bk cloud:clusters:use bplatform-eu-nl-dev-a"
alias bk-clusters-use-prod="bk cloud:clusters:use bplatform-eu-nl-prod-c"

alias bk-ins='bk sd:installations'
alias bk-ins-use-mine='bk sd:installations:use --mine'
alias bk-ins-use-prod='bk sd:installations:use kubernetes-prod'
alias bk-ins-create-dev='bk sd:installations:create dev'

alias bk-pod-rollout='bk deploy:rollout'
alias bk-pod-update='bk deploy:rollout:update'

alias bk-run-port-forwarding='bk java:local:k8s'
alias bk-run-local='./local-development/run-local.sh'
alias bk-run-spring='mvn spring-boot:run'

alias bk-perl-apply='bazel run perl-api-dev.apply'
alias bk-perl-patch='bazel run perl-api-dev.patch'

alias bk-request-dependencies='bk sd:dependencies:request -f kubernetes/harnessconfig-dev.yaml'

alias bk-iam-issue-token='curl localhost:228/s2s_auth/issue_token/$SERVER_ROLE'
alias bk-iam-whoami='curl localhost:228/identity/whoami'
alias bk-iam-sso-access='bk auth:issue-sso-access-token'
alias bk-iam-sso-refresh='bk auth:issue-sso-refresh-token'

bk_sm_rollout () {
  bk service-mesh:controlplane:rollout --project $1 --component $2
}
alias bk-sm-rollout="bk_sm_rollout"
