const plot = (el) => {
  const collectionName = el.dataset.source;
  const groupName = el.dataset.groupName;
  const testName = el.dataset.testName;

  const data = window.dataset[collectionName];
  const groupData = data.groups.find((it) => it.name === groupName);
  const testData = groupData.tests.find((it) => it.name === testName);

  const results = testData.results;
  const values = results.map((r) => r.value);
  const timestamps = results.map((r) => r.timestamp);
  const plotData = [timestamps, values];

  const opts = {
    title: testName,
    width: 1500, // FIXME: Adjust based on display size
    height: 300,
    series: [
      {},
      {
        // in-legend display
        label: testName,

        // don't connect null data points
        spanGaps: false,

        // series style
        stroke: "red",
        width: 1,
      },
    ],
  };

  const uplot = new uPlot(opts, plotData, el);
};

const fetchData = (el) => {
  const collectionName = el.dataset.source;
  const datasource = `${collectionName}.json`;
  return fetch(datasource)
    .then((response) => response.json())
    .then((data) => {
      window.dataset = { ...window.dataset, [collectionName]: data };
    })
    .catch((error) => console.error("Error fetching data:", error));
};

const promises = Array.from(
  document.querySelectorAll(".caliper-collection"),
).map(fetchData);

Promise.all(promises).then(() =>
  document.querySelectorAll(".caliper-plot").forEach(plot),
);
