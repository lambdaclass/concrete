import ConcreteSpec.Spec.GrammarData
import ConcreteSpec.Spec.GrammarVocab
import ConcreteSpec.Spec.TableVerifier

/-! Independently generated LL(1) table certificate for the early recognition
grammar (`ConcreteSpec.GrammarData.productionIds`). Stored as a balanced
sparse tree of the 1106 nonzero words out of 23815 total words so
`earlyProposal` materializes to the exact dense `Array UInt64` while kernel
proofs look up any address in logarithmic steps without reducing the dense
array or running `verifyRun` in the kernel. -/

namespace ConcreteSpec.GrammarVerification
open ConcreteSpec.GrammarData

/-- Canonical raw grammar for the full early recognition table. -/
def earlyRawGrammar : RawGrammar :=
  ⟨Nonterminal.count, ConcreteSpec.Surface0.Terminal.count, numberedGrammar productionIds⟩

theorem earlyRawGrammar_representable : representable earlyRawGrammar = true := by
  decide

/-- Balanced binary search tree mapping word addresses to nonzero `UInt64`
certificate entries. Missing addresses denote zero. -/
inductive SparseTree where
  | leaf
  | node (addr : Nat) (val : UInt64) (left right : SparseTree)
  deriving DecidableEq, Repr

/-- Materialize a sparse tree into a pre-sized word array in ascending address order. -/
def SparseTree.fill : SparseTree → Array UInt64 → Array UInt64
  | .leaf, arr => arr
  | .node addr val left right, arr =>
    right.fill ((left.fill arr).setIfInBounds addr val)

/-- Pure logarithmic-depth word lookup matching `earlyProposal.getD a 0`. -/
def SparseTree.lookup : SparseTree → Nat → UInt64
  | .leaf, _ => 0
  | .node addr val left right, x =>
    if x < addr then left.lookup x
    else if addr < x then right.lookup x
    else val

/-- Strictly ordered, nonzero-valued sparse tree bounded to `[lo, hi)`. -/
def SparseTree.bounded (lo hi : Nat) : SparseTree → Bool
  | .leaf => true
  | .node addr val left right =>
    decide (lo ≤ addr) && decide (addr < hi) && decide (val != 0) &&
      left.bounded lo addr && right.bounded (addr + 1) hi

/-- Independently generated sparse LL(1) certificate for `earlyRawGrammar`,
matching `scripts/spec/grammar_proposal.py` word for word. -/
def earlyProposalTree : SparseTree :=
  .node 5651 1
    (.node 3800 2
      (.node 1716 3
        (.node 1069 51
          (.node 429 35
            (.node 250 16
              (.node 193 7
                (.node 166 15
                  (.node 82 3
                    (.node 27 1 (.node 1 1 .leaf .leaf) .leaf)
                    (.node 108 2 .leaf .leaf))
                  (.node 191 5 (.node 169 4 .leaf .leaf) (.node 192 6 .leaf .leaf)))
                (.node 198 12
                  (.node 196 10
                    (.node 195 9 (.node 194 8 .leaf .leaf) .leaf)
                    (.node 197 11 .leaf .leaf))
                  (.node 200 14 (.node 199 13 .leaf .leaf) (.node 247 27 .leaf .leaf))))
              (.node 280 25
                (.node 276 21
                  (.node 274 19
                    (.node 273 18 (.node 272 17 .leaf .leaf) .leaf)
                    (.node 275 20 .leaf .leaf))
                  (.node 278 23 (.node 277 22 .leaf .leaf) (.node 279 24 .leaf .leaf)))
                (.node 328 30
                  (.node 326 28 (.node 281 26 .leaf .leaf) (.node 327 29 .leaf .leaf))
                  (.node 349 31 (.node 335 32 .leaf .leaf) (.node 409 33 .leaf .leaf)))))
            (.node 838 43
              (.node 673 40
                (.node 650 40
                  (.node 499 36
                    (.node 496 37 (.node 485 34 .leaf .leaf) .leaf)
                    (.node 592 38 .leaf .leaf))
                  (.node 652 40 (.node 651 40 .leaf .leaf) (.node 659 40 .leaf .leaf)))
                (.node 815 43
                  (.node 754 42
                    (.node 739 41 (.node 674 39 .leaf .leaf) .leaf)
                    (.node 814 43 .leaf .leaf))
                  (.node 831 43 (.node 818 43 .leaf .leaf) (.node 832 43 .leaf .leaf))))
              (.node 980 47
                (.node 907 44
                  (.node 900 46
                    (.node 870 43 (.node 869 43 .leaf .leaf) .leaf)
                    (.node 906 46 .leaf .leaf))
                  (.node 976 47 (.node 908 45 .leaf .leaf) (.node 977 47 .leaf .leaf)))
                (.node 1031 47
                  (.node 994 47 (.node 993 47 .leaf .leaf) (.node 1000 47 .leaf .leaf))
                  (.node 1062 51 (.node 1032 47 .leaf .leaf) (.node 1068 51 .leaf .leaf))))))
          (.node 1476 65
            (.node 1243 60
              (.node 1156 56
                (.node 1138 53
                  (.node 1072 49
                    (.node 1071 48 (.node 1070 51 .leaf .leaf) .leaf)
                    (.node 1073 50 .leaf .leaf))
                  (.node 1142 54 (.node 1139 52 .leaf .leaf) (.node 1155 59 .leaf .leaf)))
                (.node 1220 60
                  (.node 1194 58
                    (.node 1193 57 (.node 1162 55 .leaf .leaf) .leaf)
                    (.node 1219 60 .leaf .leaf))
                  (.node 1236 60 (.node 1223 60 .leaf .leaf) (.node 1237 60 .leaf .leaf))))
              (.node 1398 64
                (.node 1313 62
                  (.node 1306 63
                    (.node 1275 60 (.node 1274 60 .leaf .leaf) .leaf)
                    (.node 1312 61 .leaf .leaf))
                  (.node 1382 64 (.node 1381 64 .leaf .leaf) (.node 1385 64 .leaf .leaf)))
                (.node 1437 64
                  (.node 1405 64 (.node 1399 64 .leaf .leaf) (.node 1436 64 .leaf .leaf))
                  (.node 1474 68 (.node 1468 68 .leaf .leaf) (.node 1475 68 .leaf .leaf)))))
            (.node 1639 78
              (.node 1598 74
                (.node 1547 71
                  (.node 1543 70
                    (.node 1478 67 (.node 1477 66 .leaf .leaf) .leaf)
                    (.node 1544 69 .leaf .leaf))
                  (.node 1561 73 (.node 1560 76 .leaf .leaf) (.node 1567 72 .leaf .leaf)))
                (.node 1635 78
                  (.node 1629 78 (.node 1599 75 .leaf .leaf) (.node 1630 77 .leaf .leaf))
                  (.node 1637 78 (.node 1636 78 .leaf .leaf) (.node 1638 78 .leaf .leaf))))
              (.node 1708 1
                (.node 1704 1
                  (.node 1702 1
                    (.node 1701 2 (.node 1640 78 .leaf .leaf) .leaf)
                    (.node 1703 1 .leaf .leaf))
                  (.node 1706 1 (.node 1705 2 .leaf .leaf) (.node 1707 1 .leaf .leaf)))
                (.node 1712 1
                  (.node 1710 2 (.node 1709 1 .leaf .leaf) (.node 1711 3 .leaf .leaf))
                  (.node 1714 1 (.node 1713 2 .leaf .leaf) (.node 1715 1 .leaf .leaf)))))))
        (.node 2260 2
          (.node 1765 4
            (.node 1747 2
              (.node 1733 2
                (.node 1721 1
                  (.node 1719 1
                    (.node 1718 2 (.node 1717 1 .leaf .leaf) .leaf)
                    (.node 1720 1 .leaf .leaf))
                  (.node 1726 2 (.node 1723 2 .leaf .leaf) (.node 1730 2 .leaf .leaf)))
                (.node 1743 12
                  (.node 1740 2
                    (.node 1738 2 (.node 1735 2 .leaf .leaf) .leaf)
                    (.node 1742 2 .leaf .leaf))
                  (.node 1745 3 (.node 1744 2 .leaf .leaf) (.node 1746 3 .leaf .leaf))))
              (.node 1756 2
                (.node 1752 5
                  (.node 1750 10
                    (.node 1749 6 (.node 1748 7 .leaf .leaf) .leaf)
                    (.node 1751 2 .leaf .leaf))
                  (.node 1754 2 (.node 1753 7 .leaf .leaf) (.node 1755 5 .leaf .leaf)))
                (.node 1760 5
                  (.node 1758 7 (.node 1757 3 .leaf .leaf) (.node 1759 2 .leaf .leaf))
                  (.node 1762 3 (.node 1761 2 .leaf .leaf) (.node 1763 4 .leaf .leaf)))))
            (.node 2036 2
              (.node 1959 2
                (.node 1955 2
                  (.node 1930 2
                    (.node 1872 2 (.node 1791 4 .leaf .leaf) .leaf)
                    (.node 1933 2 .leaf .leaf))
                  (.node 1957 2 (.node 1956 2 .leaf .leaf) (.node 1958 2 .leaf .leaf)))
                (.node 1963 2
                  (.node 1961 2 (.node 1960 2 .leaf .leaf) (.node 1962 2 .leaf .leaf))
                  (.node 2011 2 (.node 1964 2 .leaf .leaf) (.node 2014 2 .leaf .leaf))))
              (.node 2045 2
                (.node 2041 2
                  (.node 2039 2
                    (.node 2038 2 (.node 2037 2 .leaf .leaf) .leaf)
                    (.node 2040 2 .leaf .leaf))
                  (.node 2043 2 (.node 2042 2 .leaf .leaf) (.node 2044 2 .leaf .leaf)))
                (.node 2113 4
                  (.node 2091 2 (.node 2090 2 .leaf .leaf) (.node 2092 2 .leaf .leaf))
                  (.node 2193 2 (.node 2173 2 .leaf .leaf) (.node 2249 2 .leaf .leaf))))))
          (.node 2983 6
            (.node 2744 4
              (.node 2595 6
                (.node 2518 4
                  (.node 2438 2
                    (.node 2356 2 (.node 2263 2 .leaf .leaf) .leaf)
                    (.node 2503 2 .leaf .leaf))
                  (.node 2579 6 (.node 2578 6 .leaf .leaf) (.node 2582 6 .leaf .leaf)))
                (.node 2671 2
                  (.node 2633 6
                    (.node 2602 6 (.node 2596 6 .leaf .leaf) .leaf)
                    (.node 2634 6 .leaf .leaf))
                  (.node 2740 4 (.node 2672 2 .leaf .leaf) (.node 2741 4 .leaf .leaf))))
              (.node 2902 2
                (.node 2796 4
                  (.node 2764 4
                    (.node 2758 4 (.node 2757 4 .leaf .leaf) .leaf)
                    (.node 2795 4 .leaf .leaf))
                  (.node 2836 2 (.node 2835 2 .leaf .leaf) (.node 2837 2 .leaf .leaf)))
                (.node 2920 2
                  (.node 2906 2 (.node 2903 2 .leaf .leaf) (.node 2919 2 .leaf .leaf))
                  (.node 2957 2 (.node 2926 2 .leaf .leaf) (.node 2958 2 .leaf .leaf)))))
            (.node 3201 4
              (.node 3077 2
                (.node 3007 6
                  (.node 3000 6
                    (.node 2987 6 (.node 2984 6 .leaf .leaf) .leaf)
                    (.node 3001 6 .leaf .leaf))
                  (.node 3039 6 (.node 3038 6 .leaf .leaf) (.node 3076 2 .leaf .leaf)))
                (.node 3162 4
                  (.node 3146 4 (.node 3145 4 .leaf .leaf) (.node 3149 4 .leaf .leaf))
                  (.node 3169 4 (.node 3163 4 .leaf .leaf) (.node 3200 4 .leaf .leaf))))
              (.node 3331 2
                (.node 3308 2
                  (.node 3242 2
                    (.node 3241 2 (.node 3240 2 .leaf .leaf) .leaf)
                    (.node 3307 2 .leaf .leaf))
                  (.node 3324 2 (.node 3311 2 .leaf .leaf) (.node 3325 2 .leaf .leaf)))
                (.node 3465 1
                  (.node 3363 2 (.node 3362 2 .leaf .leaf) (.node 3394 2 .leaf .leaf))
                  (.node 3637 2 (.node 3547 2 .leaf .leaf) (.node 3722 3 .leaf .leaf))))))))
      (.node 5404 1
        (.node 5200 1
          (.node 4618 4
            (.node 4284 2
              (.node 4116 5
                (.node 4036 4
                  (.node 4034 4
                    (.node 3965 3 (.node 3884 2 .leaf .leaf) .leaf)
                    (.node 4035 4 .leaf .leaf))
                  (.node 4057 6 (.node 4043 4 .leaf .leaf) (.node 4115 5 .leaf .leaf)))
                (.node 4197 6
                  (.node 4138 7
                    (.node 4124 5 (.node 4117 5 .leaf .leaf) .leaf)
                    (.node 4196 6 .leaf .leaf))
                  (.node 4205 6 (.node 4198 6 .leaf .leaf) (.node 4219 8 .leaf .leaf))))
              (.node 4533 5
                (.node 4452 4
                  (.node 4371 3
                    (.node 4365 3 (.node 4290 2 .leaf .leaf) .leaf)
                    (.node 4446 4 .leaf .leaf))
                  (.node 4454 4 (.node 4453 4 .leaf .leaf) (.node 4527 5 .leaf .leaf)))
                (.node 4614 5
                  (.node 4535 5 (.node 4534 5 .leaf .leaf) (.node 4608 5 .leaf .leaf))
                  (.node 4616 5 (.node 4615 5 .leaf .leaf) (.node 4617 4 .leaf .leaf)))))
            (.node 5101 6
              (.node 4940 5
                (.node 4858 4
                  (.node 4771 3
                    (.node 4690 2 (.node 4619 4 .leaf .leaf) .leaf)
                    (.node 4852 4 .leaf .leaf))
                  (.node 4933 5 (.node 4859 4 .leaf .leaf) (.node 4939 5 .leaf .leaf)))
                (.node 5023 4
                  (.node 5021 5
                    (.node 5020 5 (.node 5014 5 .leaf .leaf) .leaf)
                    (.node 5022 4 .leaf .leaf))
                  (.node 5094 6 (.node 5024 4 .leaf .leaf) (.node 5100 6 .leaf .leaf))))
              (.node 5184 1
                (.node 5166 1
                  (.node 5104 5
                    (.node 5103 5 (.node 5102 6 .leaf .leaf) .leaf)
                    (.node 5105 5 .leaf .leaf))
                  (.node 5177 1 (.node 5167 3 .leaf .leaf) (.node 5183 1 .leaf .leaf)))
                (.node 5192 1
                  (.node 5188 1 (.node 5186 1 .leaf .leaf) (.node 5190 1 .leaf .leaf))
                  (.node 5196 1 (.node 5194 1 .leaf .leaf) (.node 5198 1 .leaf .leaf))))))
          (.node 5301 1
            (.node 5244 1
              (.node 5218 1
                (.node 5210 1
                  (.node 5206 1
                    (.node 5204 1 (.node 5202 1 .leaf .leaf) .leaf)
                    (.node 5208 1 .leaf .leaf))
                  (.node 5214 1 (.node 5212 1 .leaf .leaf) (.node 5216 1 .leaf .leaf)))
                (.node 5228 1
                  (.node 5224 1
                    (.node 5222 1 (.node 5220 1 .leaf .leaf) .leaf)
                    (.node 5226 1 .leaf .leaf))
                  (.node 5232 1 (.node 5230 1 .leaf .leaf) (.node 5239 1 .leaf .leaf))))
              (.node 5279 1
                (.node 5261 1
                  (.node 5254 1
                    (.node 5253 1 (.node 5250 1 .leaf .leaf) .leaf)
                    (.node 5257 1 .leaf .leaf))
                  (.node 5269 1 (.node 5266 1 .leaf .leaf) (.node 5272 1 .leaf .leaf)))
                (.node 5289 1
                  (.node 5283 1 (.node 5282 1 .leaf .leaf) (.node 5287 1 .leaf .leaf))
                  (.node 5296 1 (.node 5292 1 .leaf .leaf) (.node 5300 1 .leaf .leaf)))))
            (.node 5349 1
              (.node 5329 1
                (.node 5317 1
                  (.node 5312 1
                    (.node 5308 1 (.node 5304 1 .leaf .leaf) .leaf)
                    (.node 5316 1 .leaf .leaf))
                  (.node 5322 1 (.node 5319 1 .leaf .leaf) (.node 5326 1 .leaf .leaf)))
                (.node 5337 1
                  (.node 5333 1 (.node 5331 1 .leaf .leaf) (.node 5335 1 .leaf .leaf))
                  (.node 5344 1 (.node 5340 1 .leaf .leaf) (.node 5348 1 .leaf .leaf))))
              (.node 5376 1
                (.node 5365 1
                  (.node 5360 1
                    (.node 5356 1 (.node 5352 1 .leaf .leaf) .leaf)
                    (.node 5364 1 .leaf .leaf))
                  (.node 5369 1 (.node 5367 1 .leaf .leaf) (.node 5373 1 .leaf .leaf)))
                (.node 5384 1
                  (.node 5380 1 (.node 5378 1 .leaf .leaf) (.node 5382 1 .leaf .leaf))
                  (.node 5398 1 (.node 5387 1 .leaf .leaf) (.node 5403 3 .leaf .leaf)))))))
        (.node 5556 1
          (.node 5478 1
            (.node 5439 1
              (.node 5421 1
                (.node 5413 1
                  (.node 5409 1
                    (.node 5407 1 (.node 5405 1 .leaf .leaf) .leaf)
                    (.node 5411 1 .leaf .leaf))
                  (.node 5417 1 (.node 5415 1 .leaf .leaf) (.node 5419 1 .leaf .leaf)))
                (.node 5431 1
                  (.node 5427 1
                    (.node 5425 1 (.node 5423 1 .leaf .leaf) .leaf)
                    (.node 5429 1 .leaf .leaf))
                  (.node 5435 1 (.node 5433 1 .leaf .leaf) (.node 5437 1 .leaf .leaf))))
              (.node 5460 1
                (.node 5449 1
                  (.node 5445 1
                    (.node 5443 1 (.node 5441 1 .leaf .leaf) .leaf)
                    (.node 5447 1 .leaf .leaf))
                  (.node 5453 1 (.node 5451 1 .leaf .leaf) (.node 5459 3 .leaf .leaf)))
                (.node 5471 1
                  (.node 5465 1 (.node 5464 3 .leaf .leaf) (.node 5470 3 .leaf .leaf))
                  (.node 5474 1 (.node 5473 3 .leaf .leaf) (.node 5475 1 .leaf .leaf)))))
            (.node 5522 1
              (.node 5508 1
                (.node 5499 3
                  (.node 5490 1
                    (.node 5487 1 (.node 5482 1 .leaf .leaf) .leaf)
                    (.node 5493 1 .leaf .leaf))
                  (.node 5503 1 (.node 5500 1 .leaf .leaf) (.node 5504 1 .leaf .leaf)))
                (.node 5516 3
                  (.node 5512 3 (.node 5510 1 .leaf .leaf) (.node 5513 1 .leaf .leaf))
                  (.node 5520 3 (.node 5517 1 .leaf .leaf) (.node 5521 1 .leaf .leaf))))
              (.node 5538 1
                (.node 5532 3
                  (.node 5528 3
                    (.node 5525 1 (.node 5524 3 .leaf .leaf) .leaf)
                    (.node 5529 1 .leaf .leaf))
                  (.node 5536 3 (.node 5533 1 .leaf .leaf) (.node 5537 1 .leaf .leaf)))
                (.node 5547 1
                  (.node 5542 3 (.node 5540 1 .leaf .leaf) (.node 5543 1 .leaf .leaf))
                  (.node 5552 1 (.node 5550 1 .leaf .leaf) (.node 5554 1 .leaf .leaf))))))
          (.node 5617 4
            (.node 5588 1
              (.node 5572 3
                (.node 5565 1
                  (.node 5561 1
                    (.node 5560 3 (.node 5558 1 .leaf .leaf) .leaf)
                    (.node 5564 3 .leaf .leaf))
                  (.node 5569 1 (.node 5568 3 .leaf .leaf) (.node 5570 1 .leaf .leaf)))
                (.node 5581 1
                  (.node 5577 1
                    (.node 5576 3 (.node 5573 1 .leaf .leaf) .leaf)
                    (.node 5580 3 .leaf .leaf))
                  (.node 5585 1 (.node 5584 3 .leaf .leaf) (.node 5586 1 .leaf .leaf))))
              (.node 5609 1
                (.node 5601 1
                  (.node 5597 1
                    (.node 5594 1 (.node 5590 1 .leaf .leaf) .leaf)
                    (.node 5599 1 .leaf .leaf))
                  (.node 5605 1 (.node 5603 1 .leaf .leaf) (.node 5608 1 .leaf .leaf)))
                (.node 5613 8
                  (.node 5611 10 (.node 5610 11 .leaf .leaf) (.node 5612 9 .leaf .leaf))
                  (.node 5615 6 (.node 5614 7 .leaf .leaf) (.node 5616 5 .leaf .leaf)))))
            (.node 5634 2
              (.node 5626 1
                (.node 5622 6
                  (.node 5620 1
                    (.node 5619 2 (.node 5618 3 .leaf .leaf) .leaf)
                    (.node 5621 7 .leaf .leaf))
                  (.node 5624 4 (.node 5623 5 .leaf .leaf) (.node 5625 3 .leaf .leaf)))
                (.node 5630 2
                  (.node 5628 2 (.node 5627 1 .leaf .leaf) (.node 5629 1 .leaf .leaf))
                  (.node 5632 2 (.node 5631 1 .leaf .leaf) (.node 5633 1 .leaf .leaf))))
              (.node 5643 1
                (.node 5639 1
                  (.node 5637 1
                    (.node 5636 2 (.node 5635 1 .leaf .leaf) .leaf)
                    (.node 5638 2 .leaf .leaf))
                  (.node 5641 1 (.node 5640 2 .leaf .leaf) (.node 5642 2 .leaf .leaf)))
                (.node 5647 1
                  (.node 5645 1 (.node 5644 2 .leaf .leaf) (.node 5646 2 .leaf .leaf))
                  (.node 5649 1 (.node 5648 2 .leaf .leaf) (.node 5650 2 .leaf .leaf)))))))))
    (.node 12719 3
      (.node 5790 3
        (.node 5721 3
          (.node 5686 3
            (.node 5669 1
              (.node 5660 2
                (.node 5656 2
                  (.node 5654 2
                    (.node 5653 1 (.node 5652 2 .leaf .leaf) .leaf)
                    (.node 5655 1 .leaf .leaf))
                  (.node 5658 2 (.node 5657 1 .leaf .leaf) (.node 5659 1 .leaf .leaf)))
                (.node 5665 1
                  (.node 5663 1
                    (.node 5662 2 (.node 5661 1 .leaf .leaf) .leaf)
                    (.node 5664 2 .leaf .leaf))
                  (.node 5667 1 (.node 5666 2 .leaf .leaf) (.node 5668 2 .leaf .leaf))))
              (.node 5678 9
                (.node 5674 2
                  (.node 5672 2
                    (.node 5671 1 (.node 5670 2 .leaf .leaf) .leaf)
                    (.node 5673 1 .leaf .leaf))
                  (.node 5676 11 (.node 5675 1 .leaf .leaf) (.node 5677 10 .leaf .leaf)))
                (.node 5682 1
                  (.node 5680 4 (.node 5679 8 .leaf .leaf) (.node 5681 3 .leaf .leaf))
                  (.node 5684 8 (.node 5683 9 .leaf .leaf) (.node 5685 4 .leaf .leaf)))))
            (.node 5704 1
              (.node 5695 3
                (.node 5691 4
                  (.node 5689 6
                    (.node 5688 7 (.node 5687 1 .leaf .leaf) .leaf)
                    (.node 5690 5 .leaf .leaf))
                  (.node 5693 1 (.node 5692 3 .leaf .leaf) (.node 5694 11 .leaf .leaf)))
                (.node 5700 1
                  (.node 5698 8
                    (.node 5697 1 (.node 5696 1 .leaf .leaf) .leaf)
                    (.node 5699 7 .leaf .leaf))
                  (.node 5702 5 (.node 5701 6 .leaf .leaf) (.node 5703 4 .leaf .leaf))))
              (.node 5713 3
                (.node 5709 1
                  (.node 5707 5
                    (.node 5706 6 (.node 5705 7 .leaf .leaf) .leaf)
                    (.node 5708 4 .leaf .leaf))
                  (.node 5711 4 (.node 5710 5 .leaf .leaf) (.node 5712 1 .leaf .leaf)))
                (.node 5717 8
                  (.node 5715 1 (.node 5714 2 .leaf .leaf) (.node 5716 9 .leaf .leaf))
                  (.node 5719 5 (.node 5718 6 .leaf .leaf) (.node 5720 4 .leaf .leaf))))))
          (.node 5756 5
            (.node 5739 1
              (.node 5730 1
                (.node 5726 1
                  (.node 5724 6
                    (.node 5723 7 (.node 5722 1 .leaf .leaf) .leaf)
                    (.node 5725 1 .leaf .leaf))
                  (.node 5728 3 (.node 5727 4 .leaf .leaf) (.node 5729 2 .leaf .leaf)))
                (.node 5735 1
                  (.node 5733 6
                    (.node 5732 1 (.node 5731 11 .leaf .leaf) .leaf)
                    (.node 5734 3 .leaf .leaf))
                  (.node 5737 6 (.node 5736 7 .leaf .leaf) (.node 5738 3 .leaf .leaf))))
              (.node 5748 5
                (.node 5744 1
                  (.node 5742 3
                    (.node 5741 6 (.node 5740 7 .leaf .leaf) .leaf)
                    (.node 5743 1 .leaf .leaf))
                  (.node 5746 3 (.node 5745 4 .leaf .leaf) (.node 5747 1 .leaf .leaf)))
                (.node 5752 5
                  (.node 5750 3 (.node 5749 4 .leaf .leaf) (.node 5751 1 .leaf .leaf))
                  (.node 5754 3 (.node 5753 4 .leaf .leaf) (.node 5755 1 .leaf .leaf)))))
            (.node 5773 2
              (.node 5765 1
                (.node 5761 2
                  (.node 5759 1
                    (.node 5758 3 (.node 5757 4 .leaf .leaf) .leaf)
                    (.node 5760 1 .leaf .leaf))
                  (.node 5763 6 (.node 5762 1 .leaf .leaf) (.node 5764 5 .leaf .leaf)))
                (.node 5769 1
                  (.node 5767 8 (.node 5766 9 .leaf .leaf) (.node 5768 2 .leaf .leaf))
                  (.node 5771 2 (.node 5770 3 .leaf .leaf) (.node 5772 1 .leaf .leaf))))
              (.node 5782 3
                (.node 5778 1
                  (.node 5776 1
                    (.node 5775 2 (.node 5774 1 .leaf .leaf) .leaf)
                    (.node 5777 2 .leaf .leaf))
                  (.node 5780 1 (.node 5779 2 .leaf .leaf) (.node 5781 6 .leaf .leaf)))
                (.node 5786 3
                  (.node 5784 7 (.node 5783 1 .leaf .leaf) (.node 5785 6 .leaf .leaf))
                  (.node 5788 7 (.node 5787 1 .leaf .leaf) (.node 5789 6 .leaf .leaf)))))))
        (.node 6808 1
          (.node 5825 1
            (.node 5808 1
              (.node 5799 1
                (.node 5795 1
                  (.node 5793 4
                    (.node 5792 1 (.node 5791 1 .leaf .leaf) .leaf)
                    (.node 5794 3 .leaf .leaf))
                  (.node 5797 4 (.node 5796 5 .leaf .leaf) (.node 5798 3 .leaf .leaf)))
                (.node 5804 5
                  (.node 5802 3
                    (.node 5801 4 (.node 5800 5 .leaf .leaf) .leaf)
                    (.node 5803 1 .leaf .leaf))
                  (.node 5806 3 (.node 5805 4 .leaf .leaf) (.node 5807 1 .leaf .leaf))))
              (.node 5817 3
                (.node 5813 9
                  (.node 5811 2
                    (.node 5810 1 (.node 5809 2 .leaf .leaf) .leaf)
                    (.node 5812 1 .leaf .leaf))
                  (.node 5815 2 (.node 5814 8 .leaf .leaf) (.node 5816 1 .leaf .leaf)))
                (.node 5821 1
                  (.node 5819 1 (.node 5818 2 .leaf .leaf) (.node 5820 2 .leaf .leaf))
                  (.node 5823 1 (.node 5822 2 .leaf .leaf) (.node 5824 2 .leaf .leaf)))))
            (.node 6348 3
              (.node 5914 1
                (.node 5830 1
                  (.node 5828 3
                    (.node 5827 1 (.node 5826 2 .leaf .leaf) .leaf)
                    (.node 5829 2 .leaf .leaf))
                  (.node 5833 3 (.node 5831 1 .leaf .leaf) (.node 5859 3 .leaf .leaf)))
                (.node 6249 1
                  (.node 6083 1 (.node 5998 1 .leaf .leaf) (.node 6165 1 .leaf .leaf))
                  (.node 6325 3 (.node 6322 3 .leaf .leaf) (.node 6347 3 .leaf .leaf))))
              (.node 6409 1
                (.node 6353 3
                  (.node 6351 3
                    (.node 6350 3 (.node 6349 3 .leaf .leaf) .leaf)
                    (.node 6352 3 .leaf .leaf))
                  (.node 6355 3 (.node 6354 3 .leaf .leaf) (.node 6356 3 .leaf .leaf)))
                (.node 6491 3
                  (.node 6483 3 (.node 6482 3 .leaf .leaf) (.node 6484 3 .leaf .leaf))
                  (.node 6572 1 (.node 6505 5 .leaf .leaf) (.node 6750 1 .leaf .leaf))))))
          (.node 11429 7
            (.node 9588 1
              (.node 8133 1
                (.node 7481 1
                  (.node 7074 3
                    (.node 6977 1 (.node 6895 1 .leaf .leaf) .leaf)
                    (.node 7297 1 .leaf .leaf))
                  (.node 7807 1 (.node 7644 1 .leaf .leaf) (.node 7970 1 .leaf .leaf)))
                (.node 8948 1
                  (.node 8622 1
                    (.node 8459 1 (.node 8296 1 .leaf .leaf) .leaf)
                    (.node 8785 1 .leaf .leaf))
                  (.node 9241 1 (.node 9076 1 .leaf .leaf) (.node 9425 1 .leaf .leaf))))
              (.node 11020 1
                (.node 10403 1
                  (.node 10077 1
                    (.node 9914 1 (.node 9751 1 .leaf .leaf) .leaf)
                    (.node 10240 1 .leaf .leaf))
                  (.node 10729 1 (.node 10566 1 .leaf .leaf) (.node 10892 1 .leaf .leaf)))
                (.node 11339 3
                  (.node 11263 3 (.node 11180 1 .leaf .leaf) (.node 11283 3 .leaf .leaf))
                  (.node 11425 7 (.node 11354 1 .leaf .leaf) (.node 11426 7 .leaf .leaf)))))
            (.node 11854 7
              (.node 11587 3
                (.node 11481 7
                  (.node 11449 7
                    (.node 11443 7 (.node 11442 7 .leaf .leaf) .leaf)
                    (.node 11480 7 .leaf .leaf))
                  (.node 11585 3 (.node 11517 1 .leaf .leaf) (.node 11586 3 .leaf .leaf)))
                (.node 11831 7
                  (.node 11748 1 (.node 11608 5 .leaf .leaf) (.node 11830 7 .leaf .leaf))
                  (.node 11847 7 (.node 11834 7 .leaf .leaf) (.node 11848 7 .leaf .leaf))))
              (.node 12239 1
                (.node 11991 3
                  (.node 11922 1
                    (.node 11886 7 (.node 11885 7 .leaf .leaf) .leaf)
                    (.node 11990 3 .leaf .leaf))
                  (.node 12013 5 (.node 11992 3 .leaf .leaf) (.node 12154 1 .leaf .leaf)))
                (.node 12477 3
                  (.node 12408 1 (.node 12321 1 .leaf .leaf) (.node 12476 3 .leaf .leaf))
                  (.node 12499 5 (.node 12478 3 .leaf .leaf) (.node 12661 3 .leaf .leaf))))))))
      (.node 17436 3
        (.node 15076 3
          (.node 13808 3
            (.node 13401 3
              (.node 13369 3
                (.node 13051 3
                  (.node 12742 5
                    (.node 12721 3 (.node 12720 3 .leaf .leaf) .leaf)
                    (.node 12964 1 .leaf .leaf))
                  (.node 13283 1 (.node 13054 3 .leaf .leaf) (.node 13297 1 .leaf .leaf)))
                (.node 13397 3
                  (.node 13395 3
                    (.node 13394 3 (.node 13372 3 .leaf .leaf) .leaf)
                    (.node 13396 3 .leaf .leaf))
                  (.node 13399 3 (.node 13398 3 .leaf .leaf) (.node 13400 3 .leaf .leaf))))
              (.node 13800 3
                (.node 13702 1
                  (.node 13551 1
                    (.node 13403 3 (.node 13402 3 .leaf .leaf) .leaf)
                    (.node 13612 1 .leaf .leaf))
                  (.node 13777 3 (.node 13774 3 .leaf .leaf) (.node 13799 3 .leaf .leaf)))
                (.node 13804 3
                  (.node 13802 3 (.node 13801 3 .leaf .leaf) (.node 13803 3 .leaf .leaf))
                  (.node 13806 3 (.node 13805 3 .leaf .leaf) (.node 13807 3 .leaf .leaf)))))
            (.node 14503 7
              (.node 14047 3
                (.node 14043 3
                  (.node 14020 3
                    (.node 14017 3 (.node 13945 1 .leaf .leaf) .leaf)
                    (.node 14042 3 .leaf .leaf))
                  (.node 14045 3 (.node 14044 3 .leaf .leaf) (.node 14046 3 .leaf .leaf)))
                (.node 14051 3
                  (.node 14049 3 (.node 14048 3 .leaf .leaf) (.node 14050 3 .leaf .leaf))
                  (.node 14267 1 (.node 14185 1 .leaf .leaf) (.node 14443 1 .leaf .leaf))))
              (.node 14663 3
                (.node 14527 7
                  (.node 14520 7
                    (.node 14507 7 (.node 14504 7 .leaf .leaf) .leaf)
                    (.node 14521 7 .leaf .leaf))
                  (.node 14559 7 (.node 14558 7 .leaf .leaf) (.node 14590 1 .leaf .leaf)))
                (.node 14686 5
                  (.node 14665 3 (.node 14664 3 .leaf .leaf) (.node 14672 3 .leaf .leaf))
                  (.node 14849 3 (.node 14753 1 .leaf .leaf) (.node 15011 1 .leaf .leaf))))))
          (.node 16464 5
            (.node 15892 3
              (.node 15658 3
                (.node 15394 3
                  (.node 15392 3
                    (.node 15319 1 (.node 15091 5 .leaf .leaf) .leaf)
                    (.node 15393 3 .leaf .leaf))
                  (.node 15415 5 (.node 15401 3 .leaf .leaf) (.node 15482 1 .leaf .leaf)))
                (.node 15817 5
                  (.node 15803 5
                    (.node 15800 5 (.node 15799 5 .leaf .leaf) .leaf)
                    (.node 15816 5 .leaf .leaf))
                  (.node 15854 5 (.node 15823 5 .leaf .leaf) (.node 15855 5 .leaf .leaf))))
              (.node 16178 5
                (.node 16127 5
                  (.node 16123 5
                    (.node 16054 1 (.node 15893 3 .leaf .leaf) .leaf)
                    (.node 16124 5 .leaf .leaf))
                  (.node 16141 5 (.node 16140 5 .leaf .leaf) (.node 16147 5 .leaf .leaf)))
                (.node 16379 1
                  (.node 16216 3 (.node 16179 5 .leaf .leaf) (.node 16217 3 .leaf .leaf))
                  (.node 16448 5 (.node 16447 5 .leaf .leaf) (.node 16451 5 .leaf .leaf)))))
            (.node 16868 3
              (.node 16775 3
                (.node 16540 3
                  (.node 16502 5
                    (.node 16471 5 (.node 16465 5 .leaf .leaf) .leaf)
                    (.node 16503 5 .leaf .leaf))
                  (.node 16771 3 (.node 16541 3 .leaf .leaf) (.node 16772 3 .leaf .leaf)))
                (.node 16826 3
                  (.node 16789 3 (.node 16788 3 .leaf .leaf) (.node 16795 3 .leaf .leaf))
                  (.node 16866 3 (.node 16827 3 .leaf .leaf) (.node 16867 3 .leaf .leaf))))
              (.node 17151 3
                (.node 17112 3
                  (.node 17096 3
                    (.node 17095 3 (.node 17028 1 .leaf .leaf) .leaf)
                    (.node 17099 3 .leaf .leaf))
                  (.node 17119 3 (.node 17113 3 .leaf .leaf) (.node 17150 3 .leaf .leaf)))
                (.node 17353 1
                  (.node 17191 3 (.node 17190 3 .leaf .leaf) (.node 17192 3 .leaf .leaf))
                  (.node 17420 3 (.node 17419 3 .leaf .leaf) (.node 17423 3 .leaf .leaf)))))))
        (.node 20428 3
          (.node 19057 1
            (.node 17839 3
              (.node 17743 3
                (.node 17514 3
                  (.node 17474 3
                    (.node 17443 3 (.node 17437 3 .leaf .leaf) .leaf)
                    (.node 17475 3 .leaf .leaf))
                  (.node 17516 3 (.node 17515 3 .leaf .leaf) (.node 17678 1 .leaf .leaf)))
                (.node 17767 3
                  (.node 17760 3
                    (.node 17747 3 (.node 17744 3 .leaf .leaf) .leaf)
                    (.node 17761 3 .leaf .leaf))
                  (.node 17799 3 (.node 17798 3 .leaf .leaf) (.node 17838 3 .leaf .leaf))))
              (.node 18570 7
                (.node 18476 1
                  (.node 18229 1
                    (.node 18068 1 (.node 17840 3 .leaf .leaf) .leaf)
                    (.node 18316 3 .leaf .leaf))
                  (.node 18554 7 (.node 18553 7 .leaf .leaf) (.node 18557 7 .leaf .leaf)))
                (.node 18609 7
                  (.node 18577 7 (.node 18571 7 .leaf .leaf) (.node 18608 7 .leaf .leaf))
                  (.node 18820 1 (.node 18639 1 .leaf .leaf) (.node 18877 1 .leaf .leaf)))))
            (.node 20015 5
              (.node 19711 5
                (.node 19688 5
                  (.node 19542 1
                    (.node 19419 1 (.node 19256 1 .leaf .leaf) .leaf)
                    (.node 19687 5 .leaf .leaf))
                  (.node 19704 5 (.node 19691 5 .leaf .leaf) (.node 19705 5 .leaf .leaf)))
                (.node 19781 3
                  (.node 19743 5 (.node 19742 5 .leaf .leaf) (.node 19780 3 .leaf .leaf))
                  (.node 20011 5 (.node 19942 1 .leaf .leaf) (.node 20012 5 .leaf .leaf))))
              (.node 20335 5
                (.node 20067 5
                  (.node 20035 5
                    (.node 20029 5 (.node 20028 5 .leaf .leaf) .leaf)
                    (.node 20066 5 .leaf .leaf))
                  (.node 20105 3 (.node 20104 3 .leaf .leaf) (.node 20267 1 .leaf .leaf)))
                (.node 20353 5
                  (.node 20339 5 (.node 20336 5 .leaf .leaf) (.node 20352 5 .leaf .leaf))
                  (.node 20390 5 (.node 20359 5 .leaf .leaf) (.node 20391 5 .leaf .leaf))))))
          (.node 21403 3
            (.node 21001 3
              (.node 20715 3
                (.node 20676 3
                  (.node 20660 3
                    (.node 20659 3 (.node 20429 3 .leaf .leaf) .leaf)
                    (.node 20663 3 .leaf .leaf))
                  (.node 20683 3 (.node 20677 3 .leaf .leaf) (.node 20714 3 .leaf .leaf)))
                (.node 20983 3
                  (.node 20756 3
                    (.node 20755 3 (.node 20754 3 .leaf .leaf) .leaf)
                    (.node 20916 1 .leaf .leaf))
                  (.node 20987 3 (.node 20984 3 .leaf .leaf) (.node 21000 3 .leaf .leaf))))
              (.node 21308 3
                (.node 21079 3
                  (.node 21039 3
                    (.node 21038 3 (.node 21007 3 .leaf .leaf) .leaf)
                    (.node 21078 3 .leaf .leaf))
                  (.node 21241 1 (.node 21080 3 .leaf .leaf) (.node 21307 3 .leaf .leaf)))
                (.node 21331 3
                  (.node 21324 3 (.node 21311 3 .leaf .leaf) (.node 21325 3 .leaf .leaf))
                  (.node 21363 3 (.node 21362 3 .leaf .leaf) (.node 21402 3 .leaf .leaf)))))
            (.node 22360 7
              (.node 21686 3
                (.node 21635 3
                  (.node 21631 3
                    (.node 21566 1 (.node 21404 3 .leaf .leaf) .leaf)
                    (.node 21632 3 .leaf .leaf))
                  (.node 21649 3 (.node 21648 3 .leaf .leaf) (.node 21655 3 .leaf .leaf)))
                (.node 21728 3
                  (.node 21726 3 (.node 21687 3 .leaf .leaf) (.node 21727 3 .leaf .leaf))
                  (.node 22117 1 (.node 21956 1 .leaf .leaf) (.node 22283 1 .leaf .leaf))))
              (.node 22627 1
                (.node 22384 7
                  (.node 22377 7
                    (.node 22364 7 (.node 22361 7 .leaf .leaf) .leaf)
                    (.node 22378 7 .leaf .leaf))
                  (.node 22416 7 (.node 22415 7 .leaf .leaf) (.node 22446 1 .leaf .leaf)))
                (.node 23226 1
                  (.node 22864 1 (.node 22684 1 .leaf .leaf) (.node 23063 1 .leaf .leaf))
                  (.node 23500 1 (.node 23349 1 .leaf .leaf) (.node 23582 1 .leaf .leaf)))))))))

/-- Dense `23815`-word LL(1) certificate array for `earlyRawGrammar`,
materialized from `earlyProposalTree`. -/
def earlyProposal : Array UInt64 :=
  earlyProposalTree.fill (Array.replicate (earlyRawGrammar.layout.scratch + 1) 0)

end ConcreteSpec.GrammarVerification
