# MixedClusteringSurvey

This repository explores algorithms for clustering **mixed-type data**, where datasets include both numerical and categorical features. It surveys existing techniques and implements several approaches for hands-on evaluation and comparison.

## 🧠 Motivation

Clustering mixed data poses unique challenges due to differing similarity measures for categorical and numerical variables. This project aims to:

* Understand the theoretical foundations behind mixed-data clustering
* Implement and experiment with popular algorithms
* Compare performance across datasets and distance measures

## 📁 Structure

```
MixedClusteringSurvey/
├── data/                   # Sample datasets
├── notebooks/              # Jupyter notebooks for analysis
├── src/                    # Clustering and preprocessing code
│   ├── preprocess.py
│   ├── distance_metrics.py
│   └── clustering.py
├── results/                # Output visualizations and metrics
├── requirements.txt        # Dependencies
└── README.md               # Project overview
```

## 🚀 Getting Started

### 1. Clone the repository

```bash
git clone https://github.com/c-daly/MixedClusteringSurvey.git
cd MixedClusteringSurvey
```

### 2. Install dependencies

Use a virtual environment for isolation:

```bash
python -m venv venv
source venv/bin/activate  # or venv\Scripts\activate on Windows
pip install -r requirements.txt
```

## 🧪 Usage

Launch the notebooks to explore clustering behavior:

```bash
jupyter notebook
```

Recommended notebooks:

* `MixedDataClustering.ipynb`: Main experimental workflow
* `DistanceFunctionComparison.ipynb`: Compare distance metrics

## 🧠 Techniques

Clustering approaches explored include:

* K-Prototypes
* Gower distance with hierarchical clustering
* Hybrid methods combining separate clustering of numeric and categorical subsets
* Distance metric engineering and preprocessing strategies

## 📚 References

The project is grounded in current academic literature on mixed data clustering and comparative studies. Key references are cited within the notebooks.

## 📝 License

This project is licensed under the MIT License.
