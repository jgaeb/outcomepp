#include "cpp11.hpp"
#include <vector>
#include <algorithm>
using namespace cpp11;

[[cpp11::register]]
writable::doubles wtd_quantile(doubles x, doubles w, doubles p) {
  int n = x.size();

  // Input validation
  if (n != w.size()) {
    stop("x and w must be the same length");
  }
  if (n == 0) {
    return writable::doubles();  // Return empty vector instead of using warn()
  }
  for (int i = 0; i < w.size(); i++) {
    if (w[i] < 0.0) {
      stop("Weights must be non-negative");
    }
  }
  for (int i = 0; i < p.size(); i++) {
    if (p[i] < 0.0 || p[i] > 1.0) {
      stop("p must be in [0, 1]");
    }
  }

  // Create index vector for sorting
  std::vector<int> indices(n);
  for (int i = 0; i < n; i++) {
    indices[i] = i;
  }

  // Sort indices based on x values
  std::sort(indices.begin(), indices.end(),
            [&x](int i1, int i2) { return x[i1] < x[i2]; });

  // Create sorted versions
  std::vector<double> x_sorted(n);
  std::vector<double> w_sorted(n);
  for (int i = 0; i < n; i++) {
    x_sorted[i] = x[indices[i]];
    w_sorted[i] = w[indices[i]];
  }

  // Calculate cumulative weights
  std::vector<double> cumw(n);
  cumw[0] = 0.0;
  for (int i = 1; i < n; i++) {
    cumw[i] = cumw[i - 1] + w_sorted[i - 1];
  }

  // Calculate total weight and check if positive
  double total_weight = cumw[n-1] + w_sorted[n-1];
  if (total_weight <= 0.0) {
    stop("Sum of weights must be positive");
  }

  // Normalize cumulative weights to [0,1]
  for (int i = 0; i < n; i++) {
    w_sorted[i] /= total_weight;
    cumw[i] /= total_weight;
  }

  // The knots take the form:
  // $$y_k = \sum_{n=0}^{k-1} w_n + \frac k {n - 1} * w_k$$
  std::vector<double> y(n);
  for (int k = 0; k < n; k++) {
    y[k] = cumw[k] + static_cast<double>(k) / (n - 1) * w_sorted[k];
  }

  // Create result vector
  writable::doubles q(p.size());

  // For each requested probability p, find appropriate interval and interpolate
  for (int i = 0; i < p.size(); i++) {
    // Find the interval containing p[i]
    int k = 0;
    while (k < n && y[k] < p[i]) {
      k++;
    }

    // Handle edge cases
    if (k == 0) {
      q[i] = x_sorted[0];
    } else if (k == n) {
      q[i] = x_sorted[n-1];
    } else {
      // Linear interpolation between x[k-1] and x[k]
      double h = (p[i] - y[k-1]) / (y[k] - y[k-1]);
      q[i] = x_sorted[k-1] + h * (x_sorted[k] - x_sorted[k-1]);
    }
  }

  return q;
}
