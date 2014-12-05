package leon.scife.wolfram

import com.wolfram.alpha.WAEngine;
import com.wolfram.alpha.WAException;
import com.wolfram.alpha.WAPlainText;
import com.wolfram.alpha.WAPod;
import com.wolfram.alpha.WAQuery;
import com.wolfram.alpha.WAQueryResult;
import com.wolfram.alpha.WASubpod;

class Querier(appid: String) {

  def request(input: String) {
    // The WAEngine is a factory for creating WAQuery objects,
    // and it also used to perform those queries. You can set properties of
    // the WAEngine (such as the desired API output format types) that will
    // be inherited by all WAQuery objects created from it. Most applications
    // will only need to crete one WAEngine object, which is used throughout
    // the life of the application.
    val engine = new WAEngine();

    // These properties will be set in all the WAQuery objects created from this WAEngine.
    engine.setAppID(appid);
    engine.addFormat("plaintext");

    // Create the query.
    val query = engine.createQuery();

    // Set properties of the query.
    query.setInput(input);

    try {
      // For educational purposes, print out the URL we are about to send:
      System.out.println("Query URL:");
      System.out.println(engine.toURL(query));
      System.out.println("");

      // This sends the URL to the Wolfram|Alpha server, gets the XML result
      // and parses it into an object hierarchy held by the WAQueryResult object.
      val queryResult = engine.performQuery(query);

      if (queryResult.isError()) {
        System.out.println("Query error");
        System.out.println("  error code: " + queryResult.getErrorCode());
        System.out.println("  error message: " + queryResult.getErrorMessage());
      } else if (!queryResult.isSuccess()) {
        System.out.println("Query was not understood; no results available.");
      } else {
        // Got a result.
        System.out.println("Successful query. Pods follow:\n");
        for (pod <- queryResult.getPods()) {
          if (!pod.isError()) {
            System.out.println(pod.getTitle());
            System.out.println("------------");
            for (subpod <- pod.getSubpods()) {
              for (element <- subpod.getContents()) {
                element match {
                  case plaintext: WAPlainText =>
                    System.out.println(plaintext.getText());
                    System.out.println("");
                  case _ =>
                }
              }
            }
            System.out.println("");
          }
        }
        // We ignored many other types of Wolfram|Alpha output, such as warnings, assumptions, etc.
        // These can be obtained by methods of WAQueryResult or objects deeper in the hierarchy.
      }
    } catch {
      case e: WAException =>
        e.printStackTrace();
    }
  }

}