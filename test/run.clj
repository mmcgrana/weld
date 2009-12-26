(set! *warn-on-reflection* true)
(use 'clj-unit.core)
(require-and-run-tests
  'weld.http-utils-test
  'weld.request-test
  'weld.response-test
  'weld.routing-test
  'weld.app-test
)
