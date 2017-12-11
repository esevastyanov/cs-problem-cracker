import org.junit.Test;

public class LargestRectangleTest
{
  @Test
  public void maximalRectangle_1() throws Exception
  {
    LargestRectangle lr = new LargestRectangle();
    final int[] heights = {2,1,5,6,2,3};
    final int largestRectangleArea = lr.largestRectangleArea(heights);

    assert largestRectangleArea == 10;
  }

  @Test
  public void maximalRectangle_2() throws Exception
  {
    LargestRectangle lr = new LargestRectangle();
    final int[] heights = {2,1,5,6,5,3};
    final int largestRectangleArea = lr.largestRectangleArea(heights);

    assert largestRectangleArea == 15;
  }

}