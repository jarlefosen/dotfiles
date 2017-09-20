# the svn plugin has to be activated for this to work.

_formatted_return_status() {
    echo '%(?:%{$fg_bold[green]%}[%T]:%{$fg_bold[red]%}[%T]%s)'
}

_formatted_kube_context() {
    echo '%{$fg_bold[blue]%}kube:(%{$fg[yellow]%}$(_kube_current_context format "namespace name project" ".")%{$fg_bold[blue]%})%{$reset_color%}'
}

_formatted_gcloud_context() {
    echo '%{$fg_bold[blue]%}gcloud:(%{$fg[green]%}$(_gcloud_current_project)%{$fg_bold[blue]%})%{$reset_color%}'
}

_formatted_uid() {
    if [ $UID -eq 0 ]; then
        echo '#'
    else
        echo '$'
    fi
}

NEWLINE=$'\n'

#RPROMPT="$(_formatted_gcloud_context) $(_formatted_kube_context)"

PROMPT="$(_formatted_return_status)"'%{$fg_bold[green]%}%p %{$fg[cyan]%}%2~ %{$fg_bold[blue]%}$(git_prompt_info)%{$fg_bold[blue]%}%{$reset_color%}'"$(_formatted_gcloud_context) $(_formatted_kube_context)"'$NEWLINE$(_formatted_uid) '


ZSH_THEME_GIT_PROMPT_PREFIX="git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%})%{$fg[yellow]%} ✗ %{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%}) "

ZSH_PROMPT_BASE_COLOR="%{$fg_bold[blue]%}"
ZSH_THEME_REPO_NAME_COLOR="%{$fg_bold[red]%}"

ZSH_THEME_SVN_PROMPT_PREFIX="svn:("
ZSH_THEME_SVN_PROMPT_SUFFIX=")"
ZSH_THEME_SVN_PROMPT_DIRTY="%{$fg[red]%} ✘ %{$reset_color%}"
ZSH_THEME_SVN_PROMPT_CLEAN=" "
