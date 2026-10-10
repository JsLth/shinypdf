// Bridges the pdf.js-based extractor (pdfjs-extract.mjs, a module script and
// so not reachable from plain scripts) to Shiny. R triggers extraction with
// session$sendCustomMessage("shinypdf-extract-images", list(...)); see
// extract_pdf_images() in R/utils.R.
if (window.Shiny) {
  Shiny.addCustomMessageHandler("shinypdf-extract-images", async function (msg) {
    const {
      id,
      url,
      inputId,
      format,
      quality,
      pages,
      min_width,
      min_height,
      inline,
      dedupe
    } = msg;

    try {
      const res = await fetch(url);
      if (!res.ok) throw new Error(`Fetching ${url} failed: HTTP ${res.status}`);
      const bytes = await res.arrayBuffer();
      const images = await window.shinypdfExtractImages(
        bytes,
        {}, // { onProgress }
        format ?? "png",
        quality ?? 0.92,
        pages ?? null,
        min_width ?? 2,
        min_height ?? 2,
        inline ?? false,
        dedupe ?? false
    );
      Shiny.setInputValue(inputId, { id, images, error: null }, { priority: "event" });
    } catch (err) {
      Shiny.setInputValue(
        inputId,
        { id, images: [], error: String(err && err.message ? err.message : err) },
        { priority: "event" }
      );
    }
  });
}
