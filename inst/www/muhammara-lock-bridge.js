import * as muhammara from "./muhammara/muhammara-wasm.js";

if (window.Shiny) {
  Shiny.addCustomMessageHandler("shinypdf-lock-pdf", async function (msg) {
    const {
      id,
      url,
      inputId,
      outPath,
      password,
      userPassword,
      ownerPassword,
      userProtectionFlag
    } = msg;

    try {
      muhammara.recrypt(url, outPath, {
        password: password,
        userPassword: userPassword,
        ownerPassword: ownerPassword,
        userProtectionFlag: userProtectionFlag
      })
      Shiny.setInputValue(inputId, { id, path: outPath, error: null }, { priority: "event" });
    } catch (err) {
      Shiny.setInputValue(
        inputId,
        { id, path: null, error: String(err && err.message ? err.message : err) },
        { priority: "event" }
      );
    }
  });

    Shiny.addCustomMessageHandler("shinypdf-unlock-pdf", async function (msg) {
      const {
        id,
        url,
        inputId,
        outPath,
        password
      } = msg;

      try {
        muhammara.recrypt(url, outPath, { password: password })
        Shiny.setInputValue(inputId, { id, path: outPath, error: null }, { priority: "event" });
      } catch (err) {
        Shiny.setInputValue(
          inputId,
          { id, path: null, error: String(err && err.message ? err.message : err) },
          { priority: "event" }
        );
      }
    });
}
