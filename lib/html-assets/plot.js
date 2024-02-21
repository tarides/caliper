const seriesColors = [
  "#7cb5ec",
  "#434348",
  "#90ed7d",
  "#f7a35c",
  "#8085e9",
  "#f15c80",
  "#e4d354",
  "#2b908f",
  "#f45b5b",
  "#91e8e1",
];

const genPlotOpts = ({
  title,
  width,
  height,
  yAxisLabel,
  series,
  commits,
  alpha = 0.3,
  prox = 5,
}) => {
  return {
    title,
    width,
    height,
    series,
    legend: {
      live: false,
    },
    focus: {
      alpha,
    },
    cursor: {
      focus: {
        prox,
      },
      drag: {
        x: true,
        y: true,
      },
    },
    scales: {
      y: {
        range: (self, dataMin, dataMax) =>
          uPlot.rangeNum(0, dataMax, 0.2, true),
      },
    },
    axes: [
      {
        grid: {
          show: true,
        },
      },
    ],
    plugins: [
      tooltipPlugin({
        onclick(u, seriesIdx, dataIdx) {
          let thisCommit = commits[dataIdx];
          // FIXME: Get project URL from caliper
          window.open(`https://github.com/ocaml/dune/commit/${thisCommit}`);
        },
        commits,
      }),
    ],
  };
};

const tooltipPlugin = ({ onclick, commits, shiftX = 10, shiftY = 10 }) => {
  let tooltipLeftOffset = 0;
  let tooltipTopOffset = 0;

  const tooltip = document.createElement("div");
  tooltip.className = "u-tooltip";

  let seriesIdx = null;
  let dataIdx = null;

  const fmtDate = uPlot.fmtDate("{YYYY}-{MM}-{DD} {h}:{mm}:{ss} {AA}");

  let over;

  let tooltipVisible = false;

  function showTooltip() {
    if (!tooltipVisible) {
      tooltip.style.display = "block";
      over.style.cursor = "pointer";
      tooltipVisible = true;
    }
  }

  function hideTooltip() {
    if (tooltipVisible) {
      tooltip.style.display = "none";
      over.style.cursor = null;
      tooltipVisible = false;
    }
  }

  function setTooltip(u) {
    showTooltip();

    let top = u.valToPos(u.data[seriesIdx][dataIdx], "y");
    let lft = u.valToPos(u.data[0][dataIdx], "x");

    tooltip.style.top = tooltipTopOffset + top + shiftX + "px";
    tooltip.style.left = tooltipLeftOffset + lft + shiftY + "px";

    tooltip.style.borderColor = seriesColors[seriesIdx - 1];
    tooltip.textContent =
      fmtDate(new Date(u.data[0][dataIdx] * 1e3)) +
      " - " +
      commits[dataIdx].slice(0, 10) +
      "\n" +
      uPlot.fmtNum(u.data[seriesIdx][dataIdx]);
  }

  return {
    hooks: {
      ready: [
        (u) => {
          over = u.over;
          tooltipLeftOffset = parseFloat(over.style.left);
          tooltipTopOffset = parseFloat(over.style.top);
          u.root.querySelector(".u-wrap").appendChild(tooltip);

          let clientX;
          let clientY;

          over.addEventListener("mousedown", (e) => {
            clientX = e.clientX;
            clientY = e.clientY;
          });

          over.addEventListener("mouseup", (e) => {
            // clicked in-place
            if (e.clientX == clientX && e.clientY == clientY) {
              if (seriesIdx != null && dataIdx != null) {
                onclick(u, seriesIdx, dataIdx);
              }
            }
          });
        },
      ],
      setCursor: [
        (u) => {
          let c = u.cursor;

          if (dataIdx != c.idx) {
            dataIdx = c.idx;

            if (seriesIdx != null) setTooltip(u);
          }
        },
      ],
      setSeries: [
        (u, sidx) => {
          if (seriesIdx != sidx) {
            seriesIdx = sidx;

            if (sidx == null) hideTooltip();
            else if (dataIdx != null) setTooltip(u);
          }
        },
      ],
    },
  };
};

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
  const commits = results.map((r) => r.commit);

  const seriesOpts = [
    {},
    {
      // in-legend display
      label: testName,

      // don't connect null data points
      spanGaps: false,

      // series style
      stroke: seriesColors[0],
      width: 1,
    },
  ];

  const plotOpts = genPlotOpts({
    title: testName,
    width: el.clientWidth,
    height: 300,
    series: seriesOpts,
    commits,
  });

  const uplot = new uPlot(plotOpts, plotData, el);
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
