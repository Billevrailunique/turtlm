set -e

PASS=0
FAIL=0
ERRORS=()

for file in sample/exemple*; do 
    exemple=$(basename "$file")
    expected="sample_ans/${exemple}.ans"

    echo "test $exemple : "

    if [ ! -f "$expected" ]; then
        echo "$exemple ignoré, pas de .ans"
        continue
    fi

    if diff -q <(_build/default/main.exe < "$file" 2>&1 | tr -d '\n' ) <(tr -d '\n' < "$expected") > /dev/null; then
        echo "[$exemple] OK"
        PASS=$((PASS + 1))
    else
        echo "[$exemple] ECHEC"
        ERRORS+=("$exemple")
        FAIL=$((FAIL + 1))
    fi
done

if [ ${#ERRORS[@]} -gt 0 ]; then
    echo "Tests échoués : ${ERRORS[*]}"
    exit 1
    else 
        echo "tout les tests sont passés"
fi

exit 0