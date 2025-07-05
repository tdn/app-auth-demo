export default [
  {
    match: {
      subject: {},
    },
    callback: {
      url: "http://resource/.mu/delta",
      method: "POST",
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
    },
  },
  {
    match: {
      predicate: { type: "uri", value: "http://mu.semte.ch/vocabularies/ext/hasFavorite" },

    },
    callback: {
      url: "http://qrencode/delta", method: "POST"
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      retry: true,
      retryTimeout: 2000
    }
  }
];
