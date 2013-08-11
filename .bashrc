function prompt {

	EXITSTATUS="$?"
	BOLD="\[\033[1m\]"
	RED="\[\033[1;31m\]"
	YELLOW="\[\033[1;33m\]"
	GREEN="\[\e[32;1m\]"
	BLUE="\[\e[34;1m\]"
	OFF="\[\033[m\]"

	PROMPT="${GREEN}\u@\h ${BLUE}\W${OFF}\n"

	if [ "${EXITSTATUS}" -eq 0 ]
	then
		PS1="${GREEN}\u ${YELLOW}in ${BLUE}\w\n${GREEN}\$ ${OFF}"
	else
		PS1="${GREEN}\u ${YELLOW}in ${BLUE}\w\n${RED}\$ ${OFF}"
	fi

	PS2="${BOLD}>${OFF} "
}

PROMPT_COMMAND=prompt
