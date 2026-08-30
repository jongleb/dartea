import * as Dartea_browser from "./Dartea_browser.mjs";
import * as Basics from "./Basics.mjs";
import * as Dict from "./Dict.mjs";
import * as Json$Decode from "./Json.Decode.mjs";
import * as Json$Encode from "./Json.Encode.mjs";
import * as Maybe from "./Maybe.mjs";
import * as Result from "./Result.mjs";
const BadUrl_ = _0 => ({ TAG: "BadUrl_", _0: _0 });
const Timeout_ = "Timeout_";
const NetworkError_ = "NetworkError_";
const BadStatus_ = (_0, _1) => ({ TAG: "BadStatus_", _0: _0, _1: _1 });
const GoodStatus_ = (_0, _1) => ({ TAG: "GoodStatus_", _0: _0, _1: _1 });
const Sending = _0 => ({ TAG: "Sending", _0: _0 });
const Receiving = _0 => ({ TAG: "Receiving", _0: _0 });
const Header = (_0, _1) => ({ _0: _0, _1: _1 });
const BadUrl = _0 => ({ TAG: "BadUrl", _0: _0 });
const Timeout = "Timeout";
const NetworkError = "NetworkError";
const BadStatus = _0 => ({ TAG: "BadStatus", _0: _0 });
const BadBody = _0 => ({ TAG: "BadBody", _0: _0 });
const prepared = (risky, r) => ({ method: r.method, headers: r.headers, url: r.url, body: r.body, expect: r.expect, timeout: r.timeout, tracker: r.tracker, allowCookiesFromOtherDomains: risky });
const get = r$1 => Dartea_browser.$$Http$request(prepared(false, { method: "GET", headers: 0, url: r$1.url, body: Dartea_browser.$$Http$emptyBody, expect: r$1.expect, timeout: Maybe.Nothing, tracker: Maybe.Nothing }));
const post = r$2 => Dartea_browser.$$Http$request(prepared(false, { method: "POST", headers: 0, url: r$2.url, body: r$2.body, expect: r$2.expect, timeout: Maybe.Nothing, tracker: Maybe.Nothing }));
const request = r$3 => Dartea_browser.$$Http$request(prepared(false, r$3));
const riskyRequest = r$4 => Dartea_browser.$$Http$request(prepared(true, r$4));
const header = (eta1, eta2) => Header(eta1, eta2);
const emptyBody = Dartea_browser.$$Http$emptyBody;
const jsonBody = value => Dartea_browser.$$Http$pair("application/json", Json$Encode.encode(0, value));
const stringBody = Dartea_browser.$$Http$pair;
const multipartBody = parts => Dartea_browser.$$Http$pair("", Dartea_browser.$$Http$toFormData(parts));
const stringPart = Dartea_browser.$$Http$pair;
const responseOf = raw => {
  const metadata = { url: raw.url, statusCode: raw.statusCode, statusText: raw.statusText, headers: Dict.fromList(raw.headers) };
  const $s1 = raw.kind;
  switch ($s1) {
    case "badUrl":
      return BadUrl_(raw.url);
    case "timeout":
      return Timeout_;
    case "network":
      return NetworkError_;
    case "bad":
      return BadStatus_(metadata, raw.body);
    default:
      return GoodStatus_(metadata, raw.body);
  }
};
const expectStringResponse = (toMsg, toResult) => Dartea_browser.$$Http$expect($s3 => Basics.composeR(responseOf, $s2 => Basics.composeR(toResult, toMsg, $s2), $s3));
const resolve = (toResult$1, response) => {
  if (response.TAG === "BadUrl_") {
    const url = response._0;
    return Result.Err(BadUrl(url));
  } else {
    if (response === "Timeout_") {
      return Result.Err(Timeout);
    } else {
      if (response === "NetworkError_") {
        return Result.Err(NetworkError);
      } else {
        if (response.TAG === "BadStatus_") {
          const metadata$1 = response._0;
          return Result.Err(BadStatus(metadata$1.statusCode));
        } else {
          const body = response._1;
          return Result.mapError(BadBody, toResult$1(body));
        }
      }
    }
  }
};
const expectString = toMsg$1 => expectStringResponse(toMsg$1, $s4 => resolve(Result.Ok, $s4));
const expectJson = (toMsg$2, decoder) => expectStringResponse(toMsg$2, $s5 => resolve(string => Result.mapError(Json$Decode.errorToString, Json$Decode.decodeString(decoder, string)), $s5));
const expectWhatever = toMsg$3 => expectStringResponse(toMsg$3, $s6 => resolve($p0 => Result.Ok(null), $s6));
const cancel = Dartea_browser.$$Http$cancel;
const track = Dartea_browser.$$Http$track;
const fractionSent = p => {
  if (p.size === 0) {
    return 1;
  } else {
    return Basics.clamp(0, 1, Basics.toFloat(p.sent) / Basics.toFloat(p.size));
  }
};
const fractionReceived = p$1 => {
  const $s7 = p$1.size;
  if ($s7 === "Nothing") {
    return 0;
  } else {
    const n = $s7._0;
    if (n === 0) {
      return 1;
    } else {
      return Basics.clamp(0, 1, Basics.toFloat(p$1.received) / Basics.toFloat(n));
    }
  }
};
const preparedTask = (risky$1, r$5) => ({ method: r$5.method, headers: r$5.headers, url: r$5.url, body: r$5.body, expect: r$5.resolver, timeout: r$5.timeout, tracker: Maybe.Nothing, allowCookiesFromOtherDomains: risky$1 });
const task = r$6 => Dartea_browser.$$Http$toTask(preparedTask(false, r$6));
const riskyTask = r$7 => Dartea_browser.$$Http$toTask(preparedTask(true, r$7));
const stringResolver = toResult$2 => Dartea_browser.$$Http$expect($s8 => Basics.composeR(responseOf, toResult$2, $s8));
const Metadata = ($a0, $a1, $a2, $a3) => ({ url: $a0, statusCode: $a1, statusText: $a2, headers: $a3 });
export { BadBody, BadStatus, BadStatus_, BadUrl, BadUrl_, GoodStatus_, Metadata, NetworkError, NetworkError_, Receiving, Sending, Timeout, Timeout_, cancel, emptyBody, expectJson, expectString, expectStringResponse, expectWhatever, fractionReceived, fractionSent, get, header, jsonBody, multipartBody, post, request, riskyRequest, riskyTask, stringBody, stringPart, stringResolver, task, track };
