concurrently \
  --kill-others \
  "wait-on client/ClientRouter.bs.js && fpack --development client/ClientRouter.bs.js --output client/build/ClientRouter $*" \
  "wait-on client/pages/IndexPage.bs.js && fpack --development client/pages/IndexPage.bs.js --output client/build/IndexPage $*" \
  "wait-on client/pages/ProjectPage.bs.js && fpack --development client/pages/ProjectPage.bs.js --output client/build/ProjectPage $*"
