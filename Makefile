check: aws-sig-v4-test aws-sig-v4-test-suite
	./aws-sig-v4-test

aws-sig-v4-test: aws-sig-v4-test.mlb aws-sig-v4-test.sml aws4client.mlb aws4client.sml aws4sml.mlb extword8vector.mlb extword8vector.sml hmac.mlb hmac.sml httpclient.mlb httpclient.sml httpheader.sml httprequest.sml uri.sml 
	mlton aws-sig-v4-test.mlb

aws-sig-v4-test-suite: aws-sig-v4-test-suite.zip
	unzip aws-sig-v4-test-suite.zip  aws-sig-v4-test-suite/*

aws-sig-v4-test-suite.zip:
	wget -nc https://docs.aws.amazon.com/ja_jp/general/latest/gr/samples/aws-sig-v4-test-suite.zip

generate: strgen.py
	mkdir -p generated
	python strgen.py

clean:
	rm -rf generated/
