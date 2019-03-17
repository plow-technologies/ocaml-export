module type Config = {let baseUrl: string;};

type projectConfig = {baseUrl: string};

let getBaseUrl: unit => string = [%bs.raw
  {|
    function () {
      var cfg = window.__frontendConfig;
      if (typeof(cfg) !== "undefined") {
        return cfg.scheme + "://" + cfg.host + ":" + cfg.port;
      } else {
        return "";
      }
    }
  |}
];

let config = {baseUrl: getBaseUrl()};