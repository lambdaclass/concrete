for i in $(seq 1 90); do
  st=$(gh run view 42451061873 --repo unbalancedparentheses/concrete2 --json status -q .status)
  [ "$st" = completed ] && break; sleep 60
done
gh run view 42451061873 --repo unbalancedparentheses/concrete2 --json conclusion,jobs -q '.conclusion, (.jobs[] | "\(.conclusion)\t\(.name)")'
