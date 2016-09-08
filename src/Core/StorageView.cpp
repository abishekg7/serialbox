//===-- Core/StorageView.cpp --------------------------------------------------------*- C++ -*-===//
//
//                                    S E R I A L B O X
//
// This file is distributed under terms of BSD license.
// See LICENSE.txt for more information
//
//===------------------------------------------------------------------------------------------===//
//
/// \file
/// This file contains the StorageView which represent a mutable view to a multi-dimensional
/// storage.
///
//===------------------------------------------------------------------------------------------===//

#include "serialbox/Core/Logging.h"
#include "serialbox/Core/StorageView.h"
#include "serialbox/Core/StorageViewIterator.h"
#include <algorithm>

namespace serialbox {

StorageView::StorageView(void* data, TypeID type, const std::vector<int>& dims,
                         const std::vector<int>& strides,
                         const std::vector<std::pair<int, int>>& padding)
    : data_(reinterpret_cast<Byte*>(data)), type_(type), dims_(dims), strides_(strides),
      padding_(padding) {
  CHECK(!dims_.empty()) << "empty dimension";
  CHECK(dims_.size() == strides_.size() && dims_.size() == padding_.size()) << "dimension mismatch";
}

StorageView::StorageView(void* data, TypeID type, std::vector<int>&& dims,
                         std::vector<int>&& strides, std::vector<std::pair<int, int>>&& padding)
    : data_(reinterpret_cast<Byte*>(data)), type_(type), dims_(dims), strides_(strides),
      padding_(padding) {
  CHECK(!dims_.empty()) << "empty dimension";
  CHECK(dims_.size() == strides_.size() && dims_.size() == padding_.size()) << "dimension mismatch";
}

void StorageView::swap(StorageView& other) noexcept {
  std::swap(data_, other.data_);
  std::swap(type_, other.type_);
  dims_.swap(other.dims_);
  strides_.swap(other.strides_);
  padding_.swap(other.padding_);
}

bool StorageView::operator==(const StorageView& right) const noexcept {
  return (data_ == right.data_ && type_ == right.type_ && dims_ == right.dims_ &&
          strides_ == right.strides_ && padding_ == right.padding_);
}

std::ostream& operator<<(std::ostream& stream, const StorageView& s) {
  stream << "StorageView [\n";
  stream << "  data = " << static_cast<void*>(s.data_) << "\n";
  stream << "  type = " << TypeUtil::toString(s.type_) << "\n";

  stream << "  dims = {";
  for(auto i : s.dims_)
    stream << " " << i;

  stream << " }\n  strides = {";
  for(auto i : s.strides_)
    stream << " " << i;

  stream << " }\n  padding = {";
  for(auto i : s.padding_)
    stream << " [" << i.first << "," << i.second << "]";

  stream << " }\n]\n";
  return stream;
}

StorageView& StorageView::operator=(StorageView other) noexcept {
  swap(other);
  return *this;
}

void swap(StorageView& a, StorageView& b) noexcept { a.swap(b); }

bool StorageView::isMemCopyable() const noexcept {

  // Check if data is contiguous in memory
  for(const auto& padPair : padding_)
    if(!(padPair.first == 0 && padPair.second == 0))
      return false;

  // Check if data is col-major
  int stride = 1;
  if(strides_[0] != 1)
    return false;

  for(std::size_t i = 1; i < dims_.size(); ++i) {
    stride *= dims_[i - 1];
    if(strides_[i] != stride)
      return false;
  }

  return true;
}

} // namespace serialbox
