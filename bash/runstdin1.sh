# printf '%s\n' "$1"

while :; do
	case "$1" in
		-p|--print)
			shift 1
			printf '%s\n' "$1"
			exit 0;
			;;
		-f|--file)
			1> ./out.txt printf '%s\n' "$1"
			exit 0;
			;;
		*)
			1>&2 printf "Invalid option ‘$1’."
			exit 1
			;;
	esac
done
