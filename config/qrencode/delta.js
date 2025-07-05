import { query, sparqlEscapeUri, update } from "mu";
import qrEncode from "../lib/qr-encode";
import { rmSync } from 'fs';

export default async function delta(req, res) {
  let affectedUris = new Set();
  for( const {inserts,deletes} of req.body )
    for ( const triple of [...inserts,...deletes] )
      if ( isFavoriteTriple(triple) && triple.object.type === "uri" )
        affectedUris.add(triple.object.value);

  for ( const resource of affectedUris ) {
    if ( await resourceIsFavorited( resource ) ) {
      // ensure QR code exists
      if ( ! await resourceHasQrCode( resource ) ) {
        const { uri: qrCodeUri } = await qrEncode(resource);
        await update(`
          PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          INSERT DATA {
            ${sparqlEscapeUri(resource)} ext:qrCode ${sparqlEscapeUri(qrCodeUri)}.
          }`);
      }
    } else {
      // remove the qr code
      if ( await resourceHasQrCode( resource ) ) {
        // remove the qr code and the share file
        const shareFile = await qrCodeSharePath( resource );
        await update(`
        PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
        DELETE {
          ${sparqlEscapeUri(resource)} ext:qrCode ?qrCode.
          ?target ?p ?o.
        } WHERE {
          ${sparqlEscapeUri(resource)} ext:qrCode ?qrCode.
          ?qrCode ^nie:dataSource? ?target.
          ?target ?p ?o.
        }`);
        if ( shareFile )
          rmSync(shareFile);
      }
    }
  }

  res.status(204).send();
}

async function qrCodeSharePath( resource ) {
  const bindings = await query(`
  PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
  PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
  SELECT ?share
  WHERE {
    ${sparqlEscapeUri(resource)} ext:qrCode/^nie:dataSource ?share.
    }`)
  const shareUri =
        bindings.length
          && bindings[0].share.type === "uri"
          && bindings[0].share.value;
  if ( shareUri )
    return `/share/${shareUri.slice("share://".length)}`;
  else
    return null;
}

function isFavoriteTriple({predicate}) {
  return predicate.value === "http://mu.semte.ch/vocabularies/ext/hasFavorite";
}

async function resourceIsFavorited(resource) {
  return (await query(`PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    ASK {
      ?thing ext:hasFavorite ${sparqlEscapeUri(resource)}.
    }`)).boolean;
}

async function resourceHasQrCode(resource) {
  return (await query(`PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
    ASK {
      ${sparqlEscapeUri(resource)} ext:qrCode ?qrCode.
    }`)).boolean;
}
