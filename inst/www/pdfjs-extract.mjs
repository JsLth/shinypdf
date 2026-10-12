import * as pdfjsLib from "./pdfjs/build/pdf.mjs";

pdfjsLib.GlobalWorkerOptions.workerSrc = new URL(
  "./pdfjs/build/pdf.worker.mjs",
  import.meta.url
).href;

const { OPS, ImageKind } = pdfjsLib;

function objsGet(objs, id) {
  return new Promise((resolve) => objs.get(id, resolve));
}

function bitmapToCanvas(bitmap) {
  const canvas = document.createElement("canvas");
  canvas.width = bitmap.width;
  canvas.height = bitmap.height;
  canvas.getContext("2d").drawImage(bitmap, 0, 0);
  return canvas;
}

async function sha256Uint8(data) {
  const digest = await crypto.subtle.digest("SHA-256", data);
  return [...new Uint8Array(digest)].map(b => b.toString(16).padStart(2, "0")).join("");
}

function canvasToRGBAData(canvas) {
  const ctx = canvas.getContext("2d");
  const { width, height } = canvas;
  const img = ctx.getImageData(0, 0, width, height);
  return img.data;
}

// Re-implements pdf.js's internal `putBinaryImageData()` for the raw-pixel
// case (used when there's no decoded ImageBitmap, e.g. odd bit depths),
// since that helper isn't part of the public API.
function rawToCanvas(imgData) {
  const { width, height, data, kind } = imgData;
  const canvas = document.createElement("canvas");
  canvas.width = width;
  canvas.height = height;
  const ctx = canvas.getContext("2d");
  const out = ctx.createImageData(width, height);
  const dest = out.data;

  if (kind === ImageKind.RGBA_32BPP) {
    dest.set(data);
  } else if (kind === ImageKind.RGB_24BPP) {
    for (let i = 0, j = 0, n = width * height; i < n; i++, j += 3) {
      dest[i * 4] = data[j];
      dest[i * 4 + 1] = data[j + 1];
      dest[i * 4 + 2] = data[j + 2];
      dest[i * 4 + 3] = 255;
    }
  } else if (kind === ImageKind.GRAYSCALE_1BPP) {
    const rowBytes = (width + 7) >> 3;
    for (let y = 0; y < height; y++) {
      const rowStart = y * rowBytes;
      for (let x = 0; x < width; x++) {
        const bit = (data[rowStart + (x >> 3)] >> (7 - (x & 7))) & 1;
        const v = bit ? 255 : 0;
        const di = (y * width + x) * 4;
        dest[di] = dest[di + 1] = dest[di + 2] = v;
        dest[di + 3] = 255;
      }
    }
  } else {
    return null;
  }
  ctx.putImageData(out, 0, 0);
  return canvas;
}

function imageObjToCanvas(imgData) {
  if (!imgData) return null;
  if (imgData instanceof ImageBitmap) return bitmapToCanvas(imgData);
  if (imgData.bitmap) return bitmapToCanvas(imgData.bitmap);
  if (imgData.data) return rawToCanvas(imgData);
  return null;
}

async function extractImagesFromPage(
  page,
  format="png",
  quality=0.92,
  min_width=2,
  min_height=2,
  inline=false,
  { dedupe = false, seenHashes = null }
) {
  const seenObjIds = new Set();
  const { fnArray, argsArray } = await page.getOperatorList();
  const out = [];
  const mime = format === "jpeg" ? "image/jpeg"
           : format.startsWith("webp") ? "image/webp"
           : "image/png";
  const q = typeof quality === "number" ? quality : 0.92;

  for (let i = 0; i < fnArray.length; i++) {
    let isImage = fnArray[i] === OPS.paintImageXObject ||
      fnArray[i] === OPS.paintImageXObjectRepeat ||
      (inline && fnArray[i] === OPS.paintInlineImageXObject);

    if (!isImage) {
      continue;
    }
    const objId = argsArray[i]?.[0];
    if (typeof objId !== "string" || seenObjIds.has(objId)) continue;
    seenObjIds.add(objId);

    // pdf.js stores objects shared across pages (e.g. repeated images) in
    // commonObjs, keyed with a "g_" prefix; everything else lives in the
    // page's own objs store.
    const store = objId.startsWith("g_") ? page.commonObjs : page.objs;
    const imgData = await objsGet(store, objId);
    const canvas = imageObjToCanvas(imgData);

    if (!canvas || canvas.width < min_width || canvas.height < min_height) continue;

    let dedupeKey = null;
    if (dedupe) {
      const rgba = canvasToRGBAData(canvas);
      dedupeKey = await sha256Uint8(rgba.buffer);
      if (seenHashes.has(dedupeKey)) continue;
      seenHashes.add(dedupeKey);
    }

    const dataUrl =
      format === "jpeg" || format === "webp"
        ? canvas.toDataURL(mime, q)
        : canvas.toDataURL(mime);

    out.push({
      objId,
      width: canvas.width,
      height: canvas.height,
      dataUrl: dataUrl,
      ...(dedupeKey ? { dedupeKey } : {})
    });
  }
  return out;
}

/**
 * Extract embedded raster images from a PDF.
 * @param {ArrayBuffer|Uint8Array|string} source Raw PDF bytes, or a URL to fetch.
 * @param {{onProgress?: (page: number, total: number) => void}} [opts]
 * @param {string} format Format of the output image. Can be "png", "jpeg", or "webp".
 * @param {number} quality Image quality of the lossy formats, i.e. jpeg and webp. Must be between 0 and 1.
 * @param {Uint8Array} pages Array of pages to extract.
 * @param {integer} min_width Minimum width of image to be extracted.
 * @param {integer} min_height Minimum height of image to be extracted.
 * @param {boolean} inline Whether inline images should be extracted.
 * @param {boolean} dedupe Whether to keep track of image hashes and skip hashes that have already been extracted.
 * @returns {Promise<Array<{page:number, index:number, objId:string, width:number, height:number, dataUrl:string}>>}
 */
export async function extractImagesFromPdf(
  source,
  { onProgress } = {},
  format="png",
  quality=0.92,
  pages=[],
  min_width=2,
  min_height=2,
  inline=false,
  dedupe=false
) {
  const isBytes = source instanceof ArrayBuffer || source instanceof Uint8Array;
  const loadingTask = pdfjsLib.getDocument({
    ...(isBytes ? { data: source } : { url: source }),
    // Needed for JBIG2/JPEG2000 decoding and ICC color conversion; harmless
    // to pass even if a given PDF doesn't use those codecs.
    wasmUrl: new URL("./pdfjs/wasm/", import.meta.url).href
  });
  const seenHashes = dedupe ? new Set() : null;

  const pdf = await loadingTask.promise;
  const images = [];
  try {
    for (let pageNum = 1; pageNum <= pdf.numPages; pageNum++) {
      if (Array.isArray(pages) && !pages.includes(pageNum)) continue;
      const page = await pdf.getPage(pageNum);
      try {
        const pageImages = await extractImagesFromPage(
          page,
          format,
          quality,
          min_width,
          min_height,
          inline,
          { dedupe, seenHashes }
        );
        pageImages.forEach((img, index) =>
          images.push({ page: pageNum, index, ...img })
        );
      } finally {
        page.cleanup();
      }
      onProgress?.(pageNum, pdf.numPages);
    }
  } finally {
    try {
      pdf.cleanup?.();
    } catch (e) {}

    try {
      loadingTask.destroy?.();
    } catch (e) {}
  }
  return images;
}

window.shinypdfExtractImages = extractImagesFromPdf;
