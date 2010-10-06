require 'visualize'
require 'test/unit'

class ConstructTest < Test::Unit::TestCase
  def mergeit t1, t2
    v1 = Visualize::Construct.fromtree(t1)
    v2 = Visualize::Construct.fromtree(t2)
    v1.merge(v2)
    #puts v1.to_a.inspect, v2.to_a.inspect
    #puts '=' * 55
    [v1, v2]
  end

  def test_simple_merge
    t1 = [1,2,3,
          [4,5,6, ['c1', :def, 'c3']],
          [7,8,9]]
    t2 = [1,2,3,
          [4,5,6, ['c2', :def, 'c3']]]

    mergeit t1, t2
  end

  def test_complex_merge
    t1 = [1,2,3, [4,5,6, [7, :def, 8],
                         [9,10,11, [12, :def, 14]]]]
    t2 = [1,2,3, [4,5,6, [7.1, :def, 8],
                         [15,16,17, [18,19,20]],
                         [9,10,11, [21,22,23, [24,25,26]],
                                   [27,28,29]]]]

    v1, v2 = mergeit t1, t2
    assert_equal(v1.to_a, [1,2,3, [4,5,6, [7, :def, 8],
                                  [9,10,11, [12, :def, 14],
                                            [21,22,23, [24,25,26]],
                                            [27,28,29]],
                                  [7.1, :def, 8],
                                  [15,16,17, [18,19,20]]]])
  end
  
end


#c = ConstructTest.new
#c.test_simple_merge
#c.test_complex_merge
