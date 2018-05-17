concurrently \
  --kill-others \
  "wait-on client/pages/IndexPage.bs.js && fpack --development client/pages/IndexPage.bs.js --output client/build/IndexPage $*" \
  "wait-on client/pages/ProjectPage.bs.js && fpack --development client/pages/ProjectPage.bs.js --output client/build/ProjectPage $*"
