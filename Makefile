check: aws-sig-v4-test aws-sig-v4-test-suite aws4clienttest uritest streamtest
	./aws-sig-v4-test
	./aws4clienttest
	./uritest
	./streamtest

aws4clienttest: aws4clienttest.mlb aws4clienttest.sml aws4client.mlb aws4client.sml aws4sml.mlb extword8vector.mlb extword8vector.sml hmac.mlb hmac.sml httpclient.mlb httpclient.sml httpheader.sml httprequest.sml uri.sml
	mlton aws4clienttest.mlb

aws-sig-v4-test: aws-sig-v4-test.mlb aws-sig-v4-test.sml aws4client.mlb aws4client.sml aws4sml.mlb extword8vector.mlb extword8vector.sml hmac.mlb hmac.sml httpclient.mlb httpclient.sml httpheader.sml httprequest.sml uri.sml 
	mlton aws-sig-v4-test.mlb

aws-sig-v4-test-suite: aws-sig-v4-test-suite.zip
	unzip aws-sig-v4-test-suite.zip  aws-sig-v4-test-suite/*

aws-sig-v4-test-suite.zip:
	wget -nc https://docs.aws.amazon.com/ja_jp/general/latest/gr/samples/aws-sig-v4-test-suite.zip

uritest: uritest.sml uritest.mlb uri.sml uri.mlb
	mlton uritest.mlb

streamtest: streamtest.sml streamtest.mlb stream.sml stream.mlb
	mlton streamtest.mlb

generate: strgen.py
	mkdir -p generated
	python strgen.py

clean:
	rm -rf generated/
	rm -rf SMLUnit/src/main/.cm/
